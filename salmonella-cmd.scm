(module salmonella-cmd ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use setup-api) ;; for installation-prefix
  (use data-structures srfi-13)
  (use salmonella salmonella-log-parser)
  (define chicken-4? #t))
 ((or chicken-5 chicken-6)
  (import (chicken base)
          (chicken file)
          (chicken fixnum)
          (chicken format)
          (chicken process signal)
          (chicken process-context)
          (chicken random)
          (chicken string)
          (chicken time))
  (import salmonella salmonella-log-parser)
  (include "libs/srfi-1.scm")
  (include "libs/srfi-13.scm")
  (define chicken-4? #f))
 (else
  (error "Unsupported CHICKEN version.")))

(include "salmonella-common.scm")

(define salmonella-log-version 3)

(define default-verbosity 2)

(define *instance-id* #f) ;; for when salmonella is called by salmonella-epidemy

(define (progress-indicator action egg verbosity #!optional egg-count total)
  (case verbosity
    ((0) "")
    ((1) (when (eq? action 'fetch)
           (print "=== " egg
                  " "
                  (if *instance-id*
                      (sprintf "(instance ~a, ~a of ~a)"
                               *instance-id*
                               egg-count
                               total)
                      (sprintf "(~a of ~a)"
                               egg-count
                               total)))))
    (else
     (let ((running (case action
                      ((fetch) (print "==== " egg " (" egg-count " of " total ") ====")
                       "  Fetching")
                      ((install) "  Installing")
                      ((check-version) "  Checking version")
                      ((test) "  Testing")
                      ((meta-data) (cond-expand
                                    (chicken-4 "  Reading .meta")
                                    ((or chicken-5 chicken-6) "  Reading .egg")))
                      ((check-dependencies) "  Checking dependencies")
                      ((check-category) "  Checking category")
                      ((check-license) "  Checking license")
                      ((check-author) "  Checking author")
                      ((check-doc) "  Checking documentation")
                      (else (error 'salmonella-progress-indicator
                                   "Invalid action"
                                   action)))))
       (display (string-pad-right running 50 #\.))
       (flush-output)))))


(define (status-reporter report verbosity)
  (case verbosity
    ((0 1) "")
    (else
     (let ((status (report-status report))
           (action (report-action report)))
       (print
        (case status
          ((0 #t) "[ ok ]")
          ((-1) "[ -- ]")
          (else "[fail]"))
        " "
        (if (or (eq? action 'check-version)
                (and (eq? action 'test)
                     (= status -1)))
            ""
            (conc (prettify-time
                   (inexact->exact (report-duration report))))))))))


(define (show-statistics log-file verbosity)
  (when (> verbosity 1)
    (let ((log (read-log-file log-file)))
      (print #<#EOF

***************************************************************************

=== Summary
Total eggs: #(count-total-eggs log)

==== Installation
Ok: #(count-install-ok log)
Failed: #(count-install-fail log)

==== Tests
Ok: #(count-test-ok log)
Failed: #(count-test-fail log)
No tests: #(count-no-test log)

==== Documentation
Documented: #(count-documented log)
Undocumented: #(count-undocumented log)

==== Total run time
#(prettify-time (inexact->exact (total-time log)))
EOF
))))


(define (check-chicken-home chicken-installation-prefix this-egg?)
  ;; Return a warning message if (chicken-home) contains Scheme files
  ;; or #f otherwise.
  (and (not chicken-installation-prefix)
       (let* ((share-dir (make-pathname (list default-installation-prefix
                                              "share")
                                        "chicken"))
              (scheme-files (glob (make-pathname share-dir "*.scm"))))
         (and (not (null? scheme-files))
              (string-append
               "======================[ W A R N I N G ]======================\n"
               "=== Scheme files have been found in " share-dir ", \n"
               "=== which is in CHICKEN's include path.  Those files may \n"
               "=== influence the test results:\n"
               (if (null? scheme-files)
                   ""
                   "===\n")
               (string-intersperse
                (map (lambda (file-path)
                       (let* ((file (pathname-strip-directory file-path))
                              (egg (case (string->symbol file)
                                     ((setup-helper.scm) "setup-helper")
                                     ((inline-type-checks.scm) "check-errors")
                                     (else #f))))
                         (sprintf "===     * ~a~a\n"
                                  file
                                  (if egg
                                      (sprintf " (~a egg)" egg)
                                      ""))))
                     scheme-files)
                "")
               (if (and this-egg? (not (null? scheme-files)))
                   (string-append
                    "===\n"
                    "=== If your egg depends on eggs that install these files,\n"
                    "=== check if you have added them to you egg's dependencies list.\n")
                   "")
               "==============================================================\n"
               )))))


(let* ((parsed-args (parse-cmd-line (command-line-arguments)
                                    '(-h
                                      --help
                                      --version
                                      (--chicken-installation-prefix)
                                      (--log-file)
                                      (--chicken-install-args)
                                      (--csi)
                                      (--csc)
                                      (--chicken-install)
                                      (--eggs-doc-dir)
                                      (--skip-eggs)
                                      (--instance-id)
                                      --keep-repo
                                      --clear-chicken-home
                                      (--repo-dir)
                                      (--verbosity))))
       (all-eggs (map string->symbol (car parsed-args)))
       (args (cdr parsed-args)))

  (handle-help args
               (lambda ()
                 (usage exit-code: 0)))

  (handle-version args)

  (let* ((this-egg? (null? all-eggs))
         (chicken-installation-prefix
          (cmd-line-arg '--chicken-installation-prefix args))
         (log-file (or (cmd-line-arg '--log-file args) "salmonella.log"))
         (chicken-install-args
          (cmd-line-arg '--chicken-install-args args))
         (csi (cmd-line-arg '--csi args))
         (csc (cmd-line-arg '--csc args))
         (chicken-install (cmd-line-arg '--chicken-instal args))
         (skip-eggs (or (and-let* ((skip (cmd-line-arg '--skip-eggs args)))
                          (map string->symbol (string-split skip ",")))
                        '()))
         (eggs (remove (lambda (egg)
                         (memq egg skip-eggs))
                       all-eggs))
         (keep-repo? (cmd-line-arg '--keep-repo args))
         (clear-chicken-home? (cmd-line-arg '--clear-chicken-home args))
         (repo-dir (and-let* ((path (cmd-line-arg '--repo-dir args)))
                     (if (absolute-pathname? path)
                         path
                         (normalize-pathname
                          (make-pathname (current-directory) path)))))
         (tmp-dir (or repo-dir (mktempdir)))
         (verbosity (or (and-let* ((verbosity (cmd-line-arg '--verbosity args)))
                          (or (string->number verbosity) default-verbosity))
                        default-verbosity)))

    (when this-egg?
      (let* ((suffix (cond-expand
                      (chicken-4 ".setup")
                      ((or chicken-5 chicken-6) ".egg")))
             (egg-spec (glob (string-append "*" suffix))))
        (cond ((null? egg-spec)
               (delete-path tmp-dir)
               (die (sprintf "Could not find a ~a file. Aborting." suffix)))
              ((null? (cdr egg-spec))
               (set! eggs (map (compose string->symbol pathname-file) egg-spec)))
              (else
               (delete-path tmp-dir)
               (die (sprintf "Found more than one ~a file.  Aborting." suffix))))))

    (when (and (null? eggs) (not this-egg?))
      (delete-path tmp-dir)
      (print "Nothing to do.")
      (exit))

    (let ((salmonella
           (make-salmonella
            tmp-dir
            eggs-doc-dir: (cmd-line-arg '--eggs-doc-dir args)
            chicken-installation-prefix: chicken-installation-prefix
            clear-chicken-home?: clear-chicken-home?
            chicken-install-args:
            (and chicken-install-args
                 (lambda (repo)
                   (list
                    (or (irregex-replace "<repo>" chicken-install-args repo)
                        chicken-install-args))))
            csi: csi
            csc: csc
            chicken-install: chicken-install
            this-egg?: this-egg?))
          (total-eggs (length eggs)))

      ;; Remove the temporary directory if interrupted
      (set-signal-handler! signal/int
                           (lambda (signal)
                             (delete-path tmp-dir)
                             (exit)))

      ;; for salmonella-epidemy
      (set! *instance-id* (cmd-line-arg '--instance-id args))

      ;; Remove old log
      (delete-file* log-file)

      ;; Initialize salmonella's repo here.  It must be done before we
      ;; try to use the environment variables, as init-repo! sets them.
      (salmonella 'init-repo!)

      (when (> verbosity 1)
        (print (salmonella 'env-info)))

      ;; Log start
      (log! (make-report #f 'start 0 (salmonella 'env-info) (current-seconds))
            log-file)

      ;; Log version
      (log! (make-report #f 'log-version 0 salmonella-log-version 0)
            log-file)

      ;; Log skipped eggs
      (for-each (lambda (egg)
                  (log! (make-report egg 'skip 0 "" 0) log-file))
                skip-eggs)

      ;; Maybe show warning about existing Scheme files in chicken-home
      (let ((msg (check-chicken-home chicken-installation-prefix this-egg?)))
        (when msg
          (with-output-to-port (current-error-port)
            (cut print msg))))

      ;; Handle all eggs
      (for-each
       (lambda (egg egg-count)

         (unless keep-repo? (salmonella 'clear-repo!))

         (salmonella 'init-repo!)

         (when (and (not chicken-4?) clear-chicken-home?)
           (salmonella 'clear-chicken-home!))

         ;; Fetch egg
         (progress-indicator 'fetch egg verbosity egg-count total-eggs)
         (let ((fetch-log (salmonella 'fetch egg)))
           (log! fetch-log log-file)
           (status-reporter fetch-log verbosity)

           (when (zero? (report-status fetch-log))

             ;; Meta data
             (progress-indicator 'meta-data egg verbosity)
             (let ((meta-log (salmonella 'meta-data egg)))
               (log! meta-log log-file)
               (status-reporter meta-log verbosity)

               (when (report-status meta-log)
                 (let ((meta-data (report-message meta-log)))

                   ;; Warnings (only logged when indicate problems)

                   ;; Check dependencies
                   (progress-indicator 'check-dependencies egg verbosity)
                   (let ((deps-log (salmonella 'check-dependencies egg meta-data)))
                     (unless (report-status deps-log)
                       (log! deps-log log-file))
                     (status-reporter deps-log verbosity))

                   ;; Check category
                   (progress-indicator 'check-category egg verbosity)
                   (let ((categ-log (salmonella 'check-category egg meta-data)))
                     (unless (report-status categ-log)
                       (log! categ-log log-file))
                     (status-reporter categ-log verbosity))

                   ;; Check license
                   (progress-indicator 'check-license egg verbosity)
                   (let ((license-log (salmonella 'check-license egg meta-data)))
                     (unless (report-status license-log)
                       (log! license-log log-file))
                     (status-reporter license-log verbosity))

                   ;; Check author
                   (progress-indicator 'check-author egg verbosity)
                   (let ((author-log (salmonella 'check-author egg meta-data)))
                     (unless (report-status author-log)
                       (log! author-log log-file))
                     (status-reporter author-log verbosity))

                   ;; Install egg
                   (progress-indicator 'install egg verbosity)
                   (let ((install-log (salmonella 'install egg)))
                     (log! install-log log-file)
                     (status-reporter install-log verbosity)

                     (when (zero? (report-status install-log))
                       ;; Check version
                       (let ((check-version-log (salmonella 'check-version egg)))
                         (progress-indicator 'check-version egg verbosity)
                         (log! check-version-log log-file)
                         (status-reporter check-version-log verbosity))

                       ;; Test egg
                       (progress-indicator 'test egg verbosity)
                       (let ((test-logs (salmonella 'test egg)))
                         (for-each (lambda (test-log)
                                     (log! test-log log-file)
                                     (when (eq? (report-action test-log) 'test)
                                       (status-reporter test-log verbosity)))
                                   test-logs)))))))))

         ;; Check doc
         (progress-indicator 'check-doc egg verbosity)
         (let ((doc-log (salmonella 'check-doc egg)))
           (log! doc-log log-file)
           (status-reporter doc-log verbosity)))

       eggs
       (iota total-eggs 1))

      (log! (make-report #f 'end 0 "" (current-seconds)) log-file)
      (unless this-egg?
        (show-statistics log-file verbosity))
      (unless keep-repo? (delete-path tmp-dir))

      (exit (if (anything-failed? log-file) 1 0)))))

) ;; end module

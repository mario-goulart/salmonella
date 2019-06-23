(import (chicken base)
        (chicken condition)
        (chicken file)
        (chicken format)
        (chicken irregex)
        (chicken pathname)
        (chicken platform)
        (chicken string)
        (chicken time posix))

(include "libs/srfi-1.scm")

(define (chicken-unit? lib major-version)
  (and
   (memq lib
         (if (< major-version 5)
             '(library eval expand data-structures ports files
               extras irregex srfi-1 srfi-4 srfi-13 srfi-14
               srfi-18 srfi-69 posix utils tcp lolevel foreign)
             '(library eval expand data-structures ports files
               extras irregex srfi-4 posix utils tcp lolevel foreign)))
   #t))

(define (egg-installed? egg repo-lib-dir)
  (let ((installed-eggs
        (map pathname-file
             (glob (make-pathname repo-lib-dir "*" "egg-info")))))
    (and (member (->string egg) installed-eggs) #t)))

;;; meta data
(define (read-egg-file egg cache-dir)
  ;; If `tmp-repo-dir' is `#f', assume this-egg
  (let* ((egg (symbol->string egg))
         (egg-file (make-pathname (and cache-dir (list cache-dir egg))
                                  egg
                                  "egg")))
    (handle-exceptions exn
      #f
      (with-input-from-file egg-file read))))



;;; Salmonella

(define (make-salmonella tmp-dir
         #!key chicken-installation-prefix
               chicken-install-args
               eggs-doc-dir
               clear-chicken-home?
               this-egg?)

  (let* ((env (salmonella-env tmp-dir
                              chicken-installation-prefix
                              chicken-install-args
                              this-egg?))
         (chicken-import-libraries
          (let* ((import-libraries-file
                  (make-pathname
                   (list (env 'chicken-installation-prefix) (env 'lib-dir))
                   "chicken-import-libraries.db"))
                 (unit-filenames
                  (handle-exceptions exn ;; FIXME: check cause of exception
                    #f
                    (with-input-from-file import-libraries-file read-list))))
            (if unit-filenames
                (map (lambda (unit)
                       (string-chomp (symbol->string unit) ".import.so"))
                     unit-filenames)
                ;; List of units before chicken-import-libraries.db
                ;; existed (this is a bit broken, as if an egg installs
                ;; a library called chicken*.import, this code is going
                ;; to copy the egg library as if it was a core library.
                (cons
                 "srfi-4"
                 (map (lambda (unit)
                        (string-chomp (pathname-strip-directory unit)
                                      ".import.so"))
                      (glob (make-pathname (list (env 'chicken-installation-prefix)
                                                 (env 'lib-dir))
                                           (string-append
                                            "chicken*.import."
                                            (if (eq? (software-type) 'windows)
                                                "dll"
                                                "so"))))))))))
    (check-chicken-executables env)

    (define (init-repo!)
      ;; Create repository-path into the test directory
      (create-directory (env 'tmp-repo-lib-dir) 'recursively)

      ;; Copy CHICKEN core units
      (for-each (lambda (unit)
                  (copy-file (make-pathname (env 'host-repository-path) unit "import.so")
                             (make-pathname (env 'tmp-repo-lib-dir) unit "import.so")
                             'clobber))
                chicken-import-libraries)

      ;; Copy types.db
      (copy-file (make-pathname (env 'host-repository-path) "types.db")
                 (make-pathname (env 'tmp-repo-lib-dir) "types.db")
                 'clobber)

      ;; Set environment variables (CHICKEN_REPOSITORY_PATH will only
      ;; be set after initializing the repository)
      (set-environment-variable! "SALMONELLA_RUNNING" "1")
      (set-environment-variable! "CHICKEN_INCLUDE_PATH" (env 'tmp-repo-share-dir))
      (set-environment-variable! "CHICKEN_C_INCLUDE_PATH"
                                 (make-pathname (env 'tmp-repo-dir) "include/chicken"))
      (set-environment-variable! "PATH" (salmonella-system-path env))
      (set-environment-variable! "CHICKEN_EGG_CACHE" (env 'cache-dir))
      (set-environment-variable! "CHICKEN_INSTALL_REPOSITORY" (env 'tmp-repo-lib-dir))
      (set-environment-variable! "CHICKEN_INSTALL_PREFIX" (env 'tmp-repo-dir))
      (set-environment-variable! "CHICKEN_REPOSITORY_PATH" (env 'tmp-repo-lib-dir)))

    (define (test-egg egg)
      ;; Run egg tests and return a list of report object.
      (let* ((start (current-seconds))
             (all-reports '())
             (add-to-reports!
              (lambda (report)
                (set! all-reports (cons report all-reports)))))
        (call/cc
         (lambda (return)
           ;; Installing test dependencies
           (let* ((meta-data (read-egg-file egg (if this-egg? #f (env 'cache-dir))))
                  (test-deps (alist-ref 'test-dependencies meta-data)))
             (let loop ((deps (remove (lambda (dep)
                                        (chicken-unit? dep (env 'major-version)))
                                      (or test-deps '()))))
               (unless (null? deps)
                 (let* ((dep-maybe-version (car deps))
                        (dep (if (pair? dep-maybe-version)
                                 (car dep-maybe-version)
                                 dep-maybe-version)))
                   (if (egg-installed? dep (env 'tmp-repo-lib-dir))
                       (loop (cdr deps))
                       (let* ((dep (if (pair? dep)
                                       (car dep)
                                       dep))
                              (fetch-log (fetch-egg dep env action: `(fetch-test-dep ,egg)))
                              (fetch-status (report-status fetch-log)))
                         (add-to-reports! fetch-log)
                         (cond ((and fetch-status (zero? fetch-status))
                                (let* ((install-log (install-egg dep env `(install-test-dep ,egg)))
                                       (install-status (report-status install-log)))
                                  (add-to-reports! install-log)
                                  (cond ((and install-status (zero? install-status))
                                         (loop (cdr deps)))
                                        (else
                                         (add-to-reports!
                                          (make-report egg 'test install-status
                                                       (string-append
                                                        (sprintf "Error installing test dependency (~a):\n\n"
                                                                 dep)
                                                        (report-message install-log))
                                                       (- (current-seconds) start)))
                                         (return (reverse all-reports))))))
                               (else
                                (add-to-reports!
                                 (make-report egg 'test fetch-status
                                              (string-append
                                               (sprintf "Error fetching test dependency (~a):\n\n"
                                                        dep)
                                               (report-message fetch-log))
                                              (- (current-seconds) start)))
                                (return (reverse all-reports))))))))))
           ;; At this point, fetching and installing test dependencies
           ;; succeeded, so proceed to run tests.
           (let* ((test-dir (make-pathname (if this-egg?
                                               #f
                                               (list (env 'cache-dir) (->string egg)))
                                           "tests"))
                  (test-script (make-pathname test-dir "run.scm")))
             (cond ((and (file-exists? test-script)
                         (file-readable? test-script))
                    (save-excursion test-dir
                      (lambda ()
                        (let ((report
                               (log-shell-command
                                egg
                                'test
                                (env 'csi)
                                `(-script run.scm
                                          ,(if (eq? (software-type) 'windows)
                                               ""
                                               "< /dev/null")))))
                          (report-duration-set! report (- (current-seconds) start))
                          (add-to-reports! report)
                          (reverse all-reports)))))
                   (else
                    (add-to-reports! (make-report egg 'test -1 "" 0))
                    (reverse all-reports))))))))

    (define (find-egg-info-files egg)
      (let ((egg-info-file (make-pathname (env 'tmp-repo-lib-dir) egg "egg-info")))
        (if (and (file-exists? egg-info-file)
                 (file-readable? egg-info-file))
            (list egg-info-file)
            ;; extension installs more than one module. Find them based
            ;; on the egg-name key in egg-info files. This feature
            ;; requires chicken > 4.6.0
            (let loop ((egg-info-files
                        (glob (make-pathname (env 'tmp-repo-lib-dir) "*.egg-info"))))
              (if (null? egg-info-files)
                  '()
                  (or (and-let* ((current-egg-info-file (car egg-info-files))
                                 (egg-info
                                  (handle-exceptions exn
                                    '()
                                    (with-input-from-file current-egg-info-file read)))
                                 (egg-name (alist-ref 'egg-name egg-info)))
                        (if (equal? (car egg-name) egg)
                            (cons current-egg-info-file
                                  (loop (cdr egg-info-files)))
                            (loop (cdr egg-info-files))))
                      (loop (cdr egg-info-files))))))))


    ;; FIXME: do still need this?  Look for egg-info and check if we
    ;; can use egg-info instead.
    (define (egg-info-version egg)

      (define (read-version egg-info-file)
        (let ((data (with-input-from-file egg-info-file read)))
          (and-let* ((version (alist-ref 'version data)))
            (car version))))

      (let ((egg-info-files (find-egg-info-files egg)))
        (cond ((null? egg-info-files) ;; no egg-info file
               #f)
              ((null? (cdr egg-info-files)) ;; a single egg-info file
               (read-version (car egg-info-files)))
              (else ;; multiple egg-info files
               (let ((versions
                      (delete-duplicates
                       (let loop ((egg-info-files egg-info-files))
                         (if (null? egg-info-files)
                             '()
                             (cons (read-version (car egg-info-files))
                                   (loop (cdr egg-info-files)))))
                       equal?)))
                 (cond ((null? versions) ;; no version
                        #f)
                       ((null? (cdr versions)) ;; a single version among all installed modules
                        (car versions))
                       (else ;; multiple versions among all installed modules (should not happen)
                        versions)))))))

    (define (check-version egg)
      ;; Check egg version and return a report object.
      ;; Status:
      ;;   0: success
      ;;   1: failure
      ;;  -1: ignore (cannot determine failure or success)
      (let ((installed-version (egg-info-version (symbol->string egg))))
        (if (list? installed-version)
            (make-report egg
                         'check-version
                         1
                         (sprintf "~a installs multiple modules with different versions" egg)
                         (string-intersperse (map ->string installed-version) " "))
            (make-report egg
                         'check-version
                         -1
                         "Version check requires a local repository of eggs sources."
                         installed-version))))

    (define (meta-data egg)
      (let ((data (read-egg-file egg (if this-egg? #f (env 'cache-dir)))))
        (make-report egg 'meta-data (and data #t) data 0)))

    (define (clear-repo! egg)
      (when (file-exists? (env 'tmp-repo-dir))
        (for-each delete-path
                  (glob (make-pathname (env 'tmp-repo-dir) "*"))))
      (delete-path (make-pathname tmp-dir egg)))

    (define (clear-chicken-home!)
      (let ((chicken-home
             (shell-command-output
              (env 'csi)
              '(-np "\"(begin (import (chicken platform)) (chicken-home))\""))))
        (for-each delete-path
                  (glob (make-pathname chicken-home "*.scm")))))

    (define (env-info)
      (define (show-envvar var)
        (string-append "  " var ": "
                       (cond ((get-environment-variable var)
                              => (lambda (val)
                                   val))
                             (else "(not set)"))))

      (define (program-version prog)
        ;; Try to obtain the program version by calling it with --version.
        ;; In case the program doesn't recognize that option, return #f.
        (let-values (((status output dur)
                      (run-shell-command prog '(--version) omit-command?: #t)))
          (and (zero? status)
               output)))

      (let* ((c-compiler (strip-surrounding-quotes
                          (shell-command-output (env 'csc) '(-cc-name))))
             (c++-compiler (strip-surrounding-quotes
                            (shell-command-output (env 'csc) '(-cxx-name))))
             (c-compiler-version (program-version c-compiler))
             (c++-compiler-version (program-version c++-compiler)))
        #<#EOF
salmonella #salmonella-version -- a tool for testing CHICKEN eggs (http://wiki.call-cc.org/egg/salmonella)

Started on #(seconds->string (current-seconds))
Command line: #(string-intersperse (argv))

Options:
  chicken-install: #(env 'chicken-install)
  repo-dir: #(env 'tmp-repo-dir)
  chicken-install-args: #((env 'chicken-install-args) (env 'tmp-repo-dir))

C compiler: #c-compiler
#c-compiler-version

C++ compiler: #c++-compiler
#c++-compiler-version

C compiler flags: #(shell-command-output (env 'csc) '(-cflags))

Linker: #(strip-surrounding-quotes (shell-command-output (env 'csc) '(-ld-name)))
Linker flags: #(shell-command-output (env 'csc) '(-ldflags))

Libraries: #(shell-command-output (env 'csc) '(-libs))

CHICKEN banner:
#(shell-command-output (env 'csi) '(-version))
Environment variables:
#(show-envvar "SALMONELLA_RUNNING")
#(show-envvar "CHICKEN_INSTALL_REPOSITORY")
#(show-envvar "CHICKEN_REPOSITORY_PATH")
#(show-envvar "CHICKEN_EGG_CACHE")
#(show-envvar "CHICKEN_INCLUDE_PATH")
#(show-envvar "CHICKEN_C_INCLUDE_PATH")
#(show-envvar "CHICKEN_HOME")
#(show-envvar "CSC_OPTIONS")
#(show-envvar "PATH")

EOF
))
    (lambda (action #!optional egg #!rest more-args)

      (case action
        ((clear-repo!) (clear-repo! egg))

        ((clear-chicken-home!) (clear-chicken-home!))

        ((init-repo!) (init-repo!))

        ((fetch) (fetch-egg egg env))

        ((install) (install-egg egg env))

        ((test) (test-egg egg))

        ((check-version) (check-version egg))

        ((env-info) (env-info))

        ((meta-data) (meta-data egg))

        ((check-dependencies) (check-dependencies egg (car more-args) (env 'major-version)))

        ((check-category) (check-category egg (car more-args)))

        ((check-doc) (check-egg-doc egg eggs-doc-dir (env 'major-version)))

        ((check-license) (check-license egg (car more-args)))

        ((check-author) (check-author egg (car more-args)))

        (else (error 'salmonella "Invalid action" action))))))

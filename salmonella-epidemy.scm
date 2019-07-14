(module salmonella-epidemy ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken foreign)
  (use data-structures srfi-13 utils)
  (use posix salmonella salmonella-log-parser))
 (chicken-5
  (import (chicken base)
          (chicken file)
          (chicken foreign)
          (chicken format)
          (chicken pathname)
          (chicken process)
          (chicken process-context)
          (chicken process signal)
          (chicken random)
          (chicken string))
  (import salmonella salmonella-log-parser)
  (include "libs/srfi-1.scm")
  (include "libs/srfi-13.scm"))
 (else
  (error "Unsupported CHICKEN version.")))

(include "salmonella-common.scm")

(define default-verbosity 0)

(define *verbosity* 0)

(define (split-eggs eggs slices)
  (let loop ((eggs eggs)
             (lists (make-list slices '())))
    (if (null? eggs)
        (map sort-eggs lists)
        (let* ((egg (car eggs))
               (first-list (car lists)))
          (set! first-list (cons egg first-list))
          (loop (cdr eggs)
                (append (cdr lists) (list first-list)))))))

(define (run-salmonella instance
                        eggs
                        chicken-installation-prefix
                        salmonella-tools-dir
                        chicken-install-args
                        skip-eggs
                        eggs-doc-dir
                        keep-repo?
                        clear-chicken-home?
                        repo-dir
                        log-dir)
  (let* ((instance-repo-dir (make-pathname repo-dir (number->string instance)))
         (cmd
          (string-intersperse
           (filter
            identity
            (list
             (make-pathname salmonella-tools-dir "salmonella")
             (and chicken-installation-prefix
                  (string-append "--chicken-installation-prefix="
                                 chicken-installation-prefix))
             (and chicken-install-args
                  (qs (string-append "--chicken-install-args=" chicken-install-args)))
             (and skip-eggs
                  (not (null? skip-eggs))
                  (string-append "--skip-eggs="
                                 (string-intersperse (map symbol->string skip-eggs) ",")))
             (and eggs-doc-dir
                  (string-append "--eggs-doc-dir=" eggs-doc-dir))
             (and keep-repo?
                  "--keep-repo")
             (string-append "--repo-dir=" instance-repo-dir)
             (and clear-chicken-home?
                  "--clear-chicken-home")
             (string-append "--log-file="
                            (make-pathname log-dir (number->string instance) "log"))
             "--verbosity=1"
             (conc "--instance-id=" instance)
             (string-intersperse (map ->string eggs)))))))
    (when (> *verbosity* 0) (print cmd))
    (process-run cmd)))


(define (merge-logs log-dir log-file instances salmonella-tools-dir)
  (let ((cmd (string-intersperse
              (list (make-pathname salmonella-tools-dir
                                   "salmonella-log-merger")
                    (string-append "--log-file=" log-file)
                    (string-intersperse
                     (map (lambda (i)
                            (make-pathname log-dir (number->string i) "log"))
                          (iota instances 1)))))))
    (when (> *verbosity* 0) (print cmd))
    (system cmd)))


(let* ((parsed-args (parse-cmd-line (command-line-arguments)
                                    '(-h
                                      --help
                                      --version
                                      (--chicken-installation-prefix)
                                      (--salmonella-tools-dir)
                                      (--log-file)
                                      (--chicken-install-args)
                                      (--eggs-doc-dir)
                                      (--skip-eggs)
                                      (--instances)
                                      (--instance-id)
                                      --keep-repo
                                      --clear-chicken-home
                                      (--repo-dir)
                                      (--verbosity))))
       (eggs (map string->symbol (car parsed-args)))
       (args (cdr parsed-args)))

  (handle-help args
               (lambda ()
                 (usage exit-code: 0 epidemy?: #t)))

  (handle-version args)

  (let* ((chicken-installation-prefix
          (cmd-line-arg '--chicken-installation-prefix args))
         (chicken-install-args
          (cmd-line-arg '--chicken-install-args args))
         (eggs-doc-dir
          (cmd-line-arg '--eggs-doc-dir args))
         (log-file (or (cmd-line-arg '--log-file args) "salmonella-epidemy.log"))
         (skip-eggs (let ((skip (cmd-line-arg '--skip-eggs args)))
                      (if skip
                          (map string->symbol (string-split skip ","))
                          '())))
         (keep-repo? (cmd-line-arg '--keep-repo args))
         (clear-chicken-home? (cmd-line-arg '--clear-chicken-home args))
         (repo-dir (or (and-let* ((path (cmd-line-arg '--repo-dir args)))
                         (if (absolute-pathname? path)
                             path
                             (normalize-pathname
                              (make-pathname (current-directory) path))))
                       (mktempdir)))
         (log-dir (or (and-let* ((path (cmd-line-arg '--log-dir args)))
                        (if (absolute-pathname? path)
                            path
                            (normalize-pathname
                             (make-pathname (current-directory) path))))
                      (mktempdir)))
         (total-eggs (length eggs))
         (instances (let ((specified-instances
                           (or (and-let* ((i (cmd-line-arg '--instances args)))
                                 (or (string->number i) 1))
                               1)))
                      (if (> specified-instances total-eggs)
                          total-eggs
                          specified-instances)))
         (salmonella-tools-dir (or (cmd-line-arg 'salmonella-tools-dir args)
                                   (foreign-value "C_TARGET_BIN_HOME" c-string))))

    (when (null? eggs)
      (print "Nothing to do.")
      (exit 0))

    ;; Remove the temporary directory if interrupted
    (set-signal-handler! signal/int
                         (lambda (signal)
                           (delete-path repo-dir)
                           (delete-path log-dir)
                           (exit)))

    (set! *verbosity*
          (or (and-let* ((verbosity (cmd-line-arg '--verbosity args)))
                (string->number verbosity))
              default-verbosity))

    ;; Remove old log
    (delete-file* log-file)

    ;; Run salmonellas
    (let ((egg-slices (split-eggs eggs instances))
          (salmonellas '())) ;; (pid . instance-id)
      (let loop ((i instances))
        (unless (zero? i)
          (when (> *verbosity* 0) (print "Running instance " i "."))
          (set! salmonellas
                (cons
                 (cons (run-salmonella i
                                       (list-ref egg-slices (- i 1))
                                       chicken-installation-prefix
                                       salmonella-tools-dir
                                       chicken-install-args
                                       skip-eggs
                                       eggs-doc-dir
                                       keep-repo?
                                       clear-chicken-home?
                                       repo-dir
                                       log-dir)
                       i)
                 salmonellas))
          (loop (- i 1))))

      ;; Wait for all salmonellas
      (let loop ((i instances))
        (unless (zero? i)
          (let-values (((pid exit-normally? exit-status) (process-wait)))
            (printf "### Instance ~a has finished.\n" (alist-ref pid salmonellas =))
            (loop (- i 1))))))

    ;; Merge logs
    (merge-logs log-dir log-file instances salmonella-tools-dir)
    (delete-path log-dir)
    (unless keep-repo?
      (delete-path repo-dir))))

) ;; end module

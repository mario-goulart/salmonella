(module salmonella-epidemy ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use data-structures srfi-13 utils)
  (use posix salmonella salmonella-log-parser))
 (chicken-5
  (import (chicken base)
          (chicken file)
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
(include "salmonella-version.scm")

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
                        salmonella-prefix
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
             (make-pathname salmonella-prefix "salmonella")
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


(define (merge-logs salmonella-prefix log-dir log-file instances)
  (let ((cmd (string-intersperse
              (list (make-pathname salmonella-prefix "salmonella-log-merger")
                    (string-append "--log-file=" log-file)
                    (string-intersperse
                     (map (lambda (i)
                            (make-pathname log-dir (number->string i) "log"))
                          (iota instances 1)))))))
    (when (> *verbosity* 0) (print cmd))
    (system cmd)))


(let ((args (command-line-arguments)))
  (when (or (member "-h" args)
            (member "--help" args))
    (usage exit-code: 0 epidemy?: #t))
  (when (member "--version" args)
    (print salmonella-version)
    (exit 0))
  (let* ((chicken-installation-prefix
          (cmd-line-arg '--chicken-installation-prefix args))
         (salmonella-prefix
          (or (cmd-line-arg '--salmonella-prefix args)
              (pathname-directory (program-name))))
         (chicken-install-args
          (cmd-line-arg '--chicken-install-args args))
         (eggs-source-dir
          (cmd-line-arg '--eggs-source-dir args))
         (eggs-doc-dir
          (cmd-line-arg '--eggs-doc-dir args))
         (log-file (or (cmd-line-arg '--log-file args) "salmonella-epidemy.log"))
         (skip-eggs (let ((skip (cmd-line-arg '--skip-eggs args)))
                      (if skip
                          (map string->symbol (string-split skip ","))
                          '())))
         (keep-repo? (and (member "--keep-repo" args) #t))
         (clear-chicken-home? (and (member "--clear-chicken-home" args) #t))
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
         (eggs (remove (lambda (egg)
                         (memq egg skip-eggs))
                       (map string->symbol
                            (remove (lambda (arg)
                                      (string-prefix? "--" arg))
                                    args))))
         (total-eggs (length eggs))
         (instances (or (and-let* ((i (cmd-line-arg '--instances args)))
                          (or (string->number i) 1))
                        1)))

    (when (null? eggs)
      (print "Nothing to do.")
      (exit 0))

    (when eggs-source-dir
      (die (pathname-strip-directory (program-name))
           " doesn't support --egg-sources-dir.  Aborting."))

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
                                       salmonella-prefix
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
    (merge-logs salmonella-prefix log-dir log-file instances)
    (delete-path log-dir)
    (unless keep-repo?
      (delete-path repo-dir))))

) ;; end module

(use posix salmonella-log-parser)
(include "salmonella-common.scm")

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
                        eggs-source-dir
                        eggs-doc-dir
                        keep-repo?
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
                  (string-append "--chicken-install-args=" chicken-install-args))
             (and skip-eggs
                  (not (null? skip-eggs))
                  (string-append "--skip-eggs=" skip-eggs))
             (and eggs-source-dir
                  (string-append "--eggs-source-dir=" eggs-source-dir))
             (and eggs-doc-dir
                  (string-append "--eggs-doc-dir=" eggs-doc-dir))
             (and keep-repo?
                  "--keep-repo")
             (string-append "--repo-dir=" instance-repo-dir)
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


(let* ((args (command-line-arguments))
       (chicken-installation-prefix
        (cmd-line-arg '--chicken-installation-prefix args))
       (salmonella-prefix
        (cmd-line-arg '--salmonella-prefix args))
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
       (repo-dir (and-let* ((path (cmd-line-arg '--repo-dir args)))
                   (if (absolute-pathname? path)
                       path
                       (normalize-pathname
                        (make-pathname (current-directory) path)))))
       (log-dir (or (and-let* ((path (cmd-line-arg '--log-dir args)))
                      (if (absolute-pathname? path)
                          path
                          (normalize-pathname
                           (make-pathname (current-directory) path))))
                    (mktempdir)))
       (tmp-dir (or repo-dir (mktempdir)))
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

  ;; Remove the temporary directory if interrupted
  (set-signal-handler! signal/int
                       (lambda (signal)
                         (delete-path tmp-dir)
                         (delete-path log-dir)
                         (exit)))

  (and-let* ((verbosity (cmd-line-arg '--verbosity args)))
    (set! *verbosity*
          (or (string->number verbosity) default-verbosity)))

  ;; Remove old log
  (delete-file* log-file)

  ;; Run salmonellas
  (let ((egg-slices (split-eggs eggs instances)))
    (let loop ((i instances))
      (unless (zero? i)
        (when (> *verbosity* 0) (print "Running instance " i "."))
        (run-salmonella i
                        (list-ref egg-slices (- i 1))
                        chicken-installation-prefix
                        salmonella-prefix
                        chicken-install-args
                        skip-eggs
                        eggs-source-dir
                        eggs-doc-dir
                        keep-repo?
                        repo-dir
                        log-dir)
        (loop (- i 1)))))

  ;; Wait for all salmonellas
  (let loop ((i instances))
    (unless (zero? i)
      (process-wait)
      (loop (- i 1))))

  ;; Merge logs
  (merge-logs salmonella-prefix log-dir log-file instances)
  (delete-path log-dir))

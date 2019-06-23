(import chicken)
(use data-structures extras files ports posix srfi-1 srfi-13)

(define (make-salmonella tmp-dir
         #!key chicken-installation-prefix
               chicken-install-args
               eggs-doc-dir
               clear-chicken-home?
               this-egg?)

  (let* ((env (salmonella-env tmp-dir
                              chicken-installation-prefix
                              chicken-install-args
                              this-egg?)))

    (check-chicken-executables env)

    ;; Set environment variables (CHICKEN_REPOSITORY will only be set
    ;; after initializing the repository)
    (setenv "SALMONELLA_RUNNING" "1")
    (setenv "CHICKEN_INSTALL_PREFIX" (env 'tmp-repo-dir))
    (setenv "CHICKEN_INCLUDE_PATH" (env 'tmp-repo-share-dir))
    (setenv "CHICKEN_C_INCLUDE_PATH" (make-pathname (env 'tmp-repo-dir) "include/chicken"))
    (setenv "PATH" (salmonella-system-path env))

    (define (init-repo!)
      ;; for create-directory/parents
      (parameterize ((setup-verbose-mode #f)
                     (run-verbose #f))
        (create-directory/parents (env 'tmp-repo-lib-dir))
        (unsetenv "CHICKEN_REPOSITORY")
        (unsetenv "CHICKEN_PREFIX")
        (let-values (((status output duration)
                      (run-shell-command (env 'chicken-install)
                                         `(-init ,(env 'tmp-repo-lib-dir)))))
          (unless (zero? status)
            (error 'init-repo! output))
          ;; Copy setup.defaults, so we can set CHICKEN_PREFIX
          (let ((setup.defaults
                 (make-pathname (list (env 'chicken-installation-prefix)
                                      "share"
                                      "chicken")
                                "setup.defaults")))
            (create-directory (env 'tmp-repo-share-dir) 'parents)
            (file-copy setup.defaults
                       (make-pathname (env 'tmp-repo-share-dir) "setup.defaults")
                       'clobber))
          ;; Only set CHICKEN_REPOSITORY and
          ;; CHICKEN_PREFIX after initializing the repo
          (setenv "CHICKEN_PREFIX" (env 'chicken-installation-prefix))
          (setenv "CHICKEN_REPOSITORY" (env 'tmp-repo-lib-dir)))))

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
           (let* ((meta-data (read-meta-file egg env))
                  (test-deps (alist-ref 'test-depends meta-data)))
             (let loop ((deps (remove (lambda (dep)
                                        (chicken-unit? dep (env 'major-version)))
                                      (or test-deps '()))))
               (unless (null? deps)
                 (let* ((dep-maybe-version (car deps))
                        (dep (if (pair? dep-maybe-version)
                                 (car dep-maybe-version)
                                 dep-maybe-version)))
                   (if (egg-installed? dep env)
                       (loop (cdr deps))
                       (let* ((fetch-log (fetch-egg dep env action: `(fetch-test-dep ,egg)))
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
                                               (list tmp-dir (->string egg)))
                                           "tests"))
                  (test-script (make-pathname test-dir "run.scm")))
             (cond ((and (file-exists? test-script)
                         (file-read-access? test-script))
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


    (define (find-setup-info-files egg)
      (let ((setup-info-file (make-pathname (env 'tmp-repo-lib-dir) egg "setup-info")))
        (if (file-read-access? setup-info-file)
            (list setup-info-file)
            ;; extension installs more than one module. Find them based
            ;; on the egg-name key in setup-info files. This feature
            ;; requires chicken > 4.6.0
            (let loop ((setup-info-files
                        (glob (make-pathname (env 'tmp-repo-lib-dir) "*.setup-info"))))
              (if (null? setup-info-files)
                  '()
                  (or (and-let* ((current-setup-info-file (car setup-info-files))
                                 (setup-info
                                  (handle-exceptions exn
                                    '()
                                    (with-input-from-file current-setup-info-file read)))
                                 (egg-name (alist-ref 'egg-name setup-info)))
                        (if (equal? (car egg-name) egg)
                            (cons current-setup-info-file
                                  (loop (cdr setup-info-files)))
                            (loop (cdr setup-info-files))))
                      (loop (cdr setup-info-files))))))))


    (define (setup-info-version egg)

      (define (read-version setup-info-file)
        (let ((data (with-input-from-file setup-info-file read)))
          (and-let* ((version (alist-ref 'version data)))
            (car version))))

      (let ((setup-info-files (find-setup-info-files egg)))
        (cond ((null? setup-info-files) ;; no setup-info file
               #f)
              ((null? (cdr setup-info-files)) ;; a single setup-info file
               (read-version (car setup-info-files)))
              (else ;; multiple setup-info files
               (let ((versions
                      (delete-duplicates
                       (let loop ((setup-info-files setup-info-files))
                         (if (null? setup-info-files)
                             '()
                             (cons (read-version (car setup-info-files))
                                   (loop (cdr setup-info-files)))))
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
      (let ((installed-version (setup-info-version (symbol->string egg))))
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
      (let ((data (read-meta-file egg env)))
        (make-report egg 'meta-data (and data #t) data 0)))

    (define (clear-repo! egg)
      (when (file-exists? (env 'tmp-repo-dir))
        (for-each delete-path
                  (glob (make-pathname (env 'tmp-repo-dir) "*"))))
      (delete-path (make-pathname tmp-dir egg))

      (when clear-chicken-home?
        (let ((chicken-home
               (shell-command-output (env 'csi) '(-np "\"(chicken-home)\""))))
          (for-each delete-path
                    (glob (make-pathname chicken-home "*.scm"))))))

    (define (env-info)
      (define (show-envvar var #!optional value)
        (string-append "  " var ": "
                       (or value
                           (cond ((get-environment-variable var)
                                  => (lambda (val)
                                       val))
                                 (else "(not set)")))))

      (define (program-version prog)
        ;; Try to obtain the program version by calling it with --version.
        ;; In case the program doesn't recognize that option, return #f.
        (let-values (((status output dur)
                      (run-shell-command prog '(--version) omit-command?: #t)))
          (and (zero? status)
               (string-trim-both output))))

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
#(show-envvar "CHICKEN_PREFIX" (env 'chicken-installation-prefix))
#(show-envvar "CHICKEN_INSTALL_PREFIX")
#(show-envvar "CHICKEN_INCLUDE_PATH")
#(show-envvar "CHICKEN_C_INCLUDE_PATH")
#(show-envvar "CHICKEN_REPOSITORY" (env 'tmp-repo-lib-dir))
#(show-envvar "CHICKEN_HOME")
#(show-envvar "CSC_OPTIONS")
#(show-envvar "PATH")

EOF
)) ;; Beware of the hack above.  CHICKEN_REPOSITORY and CHICKEN_PREFIX
   ;; are only set by salmonella after `init-repo!' is called.  Here we
   ;; print their value but the environment variable may not be
   ;; actually set, since `env-info' can be called before `init-repo!'.


    (lambda (action #!optional egg #!rest more-args)

      (case action
        ((clear-repo!) (clear-repo! egg))

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

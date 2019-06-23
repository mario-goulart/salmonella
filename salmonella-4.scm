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

    (lambda (action #!optional egg #!rest more-args)
      (case action
        ((clear-repo!) (clear-repo! egg))
        ((init-repo!) (init-repo!))
        ((fetch) (fetch-egg egg env))
        ((install) (install-egg egg env))
        ((test) (test-egg egg env))
        ((check-version) (check-version egg env))
        ((env-info) (env-info env))
        ((meta-data) (meta-data egg env))
        ((check-dependencies) (check-dependencies egg (car more-args) (env 'major-version)))
        ((check-category) (check-category egg (car more-args)))
        ((check-doc) (check-egg-doc egg eggs-doc-dir (env 'major-version)))
        ((check-license) (check-license egg (car more-args)))
        ((check-author) (check-author egg (car more-args)))
        (else (error 'salmonella "Invalid action" action))))))

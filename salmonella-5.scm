(import (chicken base)
        (chicken condition)
        (chicken pathname))

(define (make-salmonella tmp-dir
         #!key chicken-installation-prefix
               chicken-install-args
               eggs-doc-dir
               clear-chicken-home?
               this-egg?)

  (let* ((env (salmonella-env tmp-dir
                              chicken-installation-prefix
                              chicken-install-args
                              this-egg?
                              clear-chicken-home?))
         (chicken-import-libraries
          ;; List of chicken import libraries.  This is a bit broken,
          ;; as if an egg installs a library called chicken*.import,
          ;; this code is going to copy the egg library as if it was a
          ;; core library.
          (cons
           "srfi-4"
           (map (lambda (unit)
                  (string-chomp (pathname-strip-directory unit)
                                ".import.so"))
                (glob (make-pathname (env 'host-repository-path)
                                     (string-append
                                      "chicken*.import."
                                      (if (eq? (software-type) 'windows)
                                          "dll"
                                          "so"))))))))
    (check-chicken-executables env)

    (define (init-repo!)
      ;; Create repository-path into the test directory
      (create-directory (env 'tmp-repo-lib-dir) 'recursively)

      ;; Copy CHICKEN executables
      (init-tmp-repo-bin-dir! env)

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

      ;; Set environment variables (CHICKEN_REPOSITORY_PATH must be
      ;; set after initializing the repository)
      (set-environment-variable! "SALMONELLA_RUNNING" "1")
      (set-environment-variable! "CHICKEN_INCLUDE_PATH" (env 'tmp-repo-share-dir))
      (set-environment-variable! "CHICKEN_C_INCLUDE_PATH"
                                 (make-pathname (env 'tmp-repo-dir) "include/chicken"))
      (set-environment-variable! "PATH" (salmonella-system-path env))
      (set-environment-variable! "CHICKEN_EGG_CACHE" (env 'cache-dir))
      (set-environment-variable! "CHICKEN_INSTALL_REPOSITORY" (env 'tmp-repo-lib-dir))
      (set-environment-variable! "CHICKEN_INSTALL_PREFIX" (env 'tmp-repo-dir))
      (set-environment-variable! "CHICKEN_REPOSITORY_PATH" (env 'tmp-repo-lib-dir)))

    (lambda (action #!optional egg #!rest more-args)
      (case action
        ((clear-repo!) (clear-repo! egg env))
        ((clear-chicken-home!) (clear-chicken-home! env))
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

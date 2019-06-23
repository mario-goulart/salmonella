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

        ((test) (test-egg egg env))

        ((check-version) (check-version egg))

        ((env-info) (env-info))

        ((meta-data) (meta-data egg env))

        ((check-dependencies) (check-dependencies egg (car more-args) (env 'major-version)))

        ((check-category) (check-category egg (car more-args)))

        ((check-doc) (check-egg-doc egg eggs-doc-dir (env 'major-version)))

        ((check-license) (check-license egg (car more-args)))

        ((check-author) (check-author egg (car more-args)))

        (else (error 'salmonella "Invalid action" action))))))

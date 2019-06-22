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

  (let* ((mingw? (eq? (build-platform) 'mingw32))
         (chicken-installation-prefix
          (or chicken-installation-prefix default-installation-prefix))
         (chicken-install-args
          (or chicken-install-args
              (lambda (repo-dir)
                '(-v -test))))
         (chicken-install
          (make-pathname (list chicken-installation-prefix "bin")
                         "chicken-install"
                         (and mingw? "exe")))
        (csi (make-pathname (list chicken-installation-prefix "bin")
                            "csi"
                            (and mingw? "exe")))
        (csc (make-pathname (list chicken-installation-prefix "bin")
                            "csc"
                            (and mingw? "exe")))
        (tmp-repo-dir (make-pathname tmp-dir "repo"))
        (host-repository-path
         (shell-command-output chicken-install '(-repository)))
        (binary-version (pathname-file host-repository-path))
        (major-version
         (let ((v (shell-command-output
                   csi
                   '(-np "\"(begin (import chicken.platform) (chicken-version))\""))))
           (string->number (car (string-split v ".")))))
        (lib-dir (make-pathname '("lib" "chicken") binary-version))
        (cache-dir (make-pathname tmp-repo-dir "cache"))
        (tmp-repo-lib-dir (make-pathname tmp-repo-dir lib-dir))
        (tmp-repo-share-dir
         (make-pathname (list tmp-repo-dir "share") "chicken"))
        (chicken-import-libraries
         (let* ((import-libraries-file
                 (make-pathname
                  (list chicken-installation-prefix "lib" "chicken" binary-version)
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
		     (glob (make-pathname (list chicken-installation-prefix
						lib-dir)
					  (string-append
					   "chicken*.import."
					   (if (eq? (software-type) 'windows)
					       "dll"
					       "so"))))))))))
    (for-each (lambda (file)
                (unless (file-executable? file)
                  (error 'make-salmonella
                         (conc file " cannot be found or have no execute access."))))
              (list chicken-install csi))

    (define (log-shell-command egg action command args)
      (let-values (((status output duration) (run-shell-command command args)))
        (make-report egg action status output duration)))

    (define (init-repo!)
      ;; Create repository-path into the test directory
      (create-directory tmp-repo-lib-dir 'recursively)

      ;; Copy CHICKEN core units
      (for-each (lambda (unit)
                  (copy-file (make-pathname host-repository-path unit "import.so")
                             (make-pathname tmp-repo-lib-dir unit "import.so")
                             'clobber))
                chicken-import-libraries)

      ;; Copy types.db
      (copy-file (make-pathname host-repository-path "types.db")
                 (make-pathname tmp-repo-lib-dir "types.db")
                 'clobber)

      ;; Set environment variables (CHICKEN_REPOSITORY_PATH will only
      ;; be set after initializing the repository)
      (set-environment-variable! "SALMONELLA_RUNNING" "1")
      (set-environment-variable! "CHICKEN_INCLUDE_PATH" tmp-repo-share-dir)
      (set-environment-variable! "CHICKEN_C_INCLUDE_PATH"
                                 (make-pathname tmp-repo-dir "include/chicken"))
      (set-environment-variable! "PATH"
                                 (string-intersperse
                                  (list (make-pathname tmp-repo-dir "bin")
                                        (make-pathname chicken-installation-prefix "bin")
                                        (get-environment-variable "PATH"))
                                  (if (eq? (software-type) 'windows)
                                      ";"
                                      ":")))
      (set-environment-variable! "CHICKEN_EGG_CACHE" cache-dir)
      (set-environment-variable! "CHICKEN_INSTALL_REPOSITORY" tmp-repo-lib-dir)
      (set-environment-variable! "CHICKEN_INSTALL_PREFIX" tmp-repo-dir)
      (set-environment-variable! "CHICKEN_REPOSITORY_PATH" tmp-repo-lib-dir))

    (define (fetch-egg egg #!key (action 'fetch) version)
      ;; Fetches egg and returns a report object
      (save-excursion tmp-dir
        (lambda ()
          (if (and this-egg? (eq? action 'fetch)) ;; don't fetch this egg
              (make-report egg 'fetch 0 "" 0)
              (log-shell-command egg
                                 action
                                 chicken-install
                                 `(-r ,@(chicken-install-args tmp-repo-dir)
                                      ,(if version
                                           (conc egg ":" version)
                                           egg)))))))

    (define (install-egg egg #!optional (action 'install))
      ;; Installs egg and returns a report object
      (let ((install
             (lambda ()
               (log-shell-command
                egg
                action
                chicken-install
                `(,@(delete '-test (chicken-install-args tmp-repo-dir)))))))
        (if (and this-egg? (eq? action 'install)) ;; install this egg from this dir
            (install)
            (save-excursion (make-pathname cache-dir (->string egg)) install))))


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
           (let* ((meta-data (read-egg-file egg (if this-egg? #f cache-dir)))
                  (test-deps (alist-ref 'test-dependencies meta-data)))
             (let loop ((deps (remove (lambda (dep)
                                        (chicken-unit? dep major-version))
                                      (or test-deps '()))))
               (unless (null? deps)
                 (let* ((dep-maybe-version (car deps))
                        (dep (if (pair? dep-maybe-version)
                                 (car dep-maybe-version)
                                 dep-maybe-version)))
                   (if (egg-installed? dep tmp-repo-lib-dir)
                       (loop (cdr deps))
                       (let* ((dep (if (pair? dep)
                                       (car dep)
                                       dep))
                              (fetch-log (fetch-egg dep action: `(fetch-test-dep ,egg)))
                              (fetch-status (report-status fetch-log)))
                         (add-to-reports! fetch-log)
                         (cond ((and fetch-status (zero? fetch-status))
                                (let* ((install-log (install-egg dep `(install-test-dep ,egg)))
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
                                               (list cache-dir (->string egg)))
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
                                csi
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
      (let ((egg-info-file (make-pathname tmp-repo-lib-dir egg "egg-info")))
        (if (and (file-exists? egg-info-file)
                 (file-readable? egg-info-file))
            (list egg-info-file)
            ;; extension installs more than one module. Find them based
            ;; on the egg-name key in egg-info files. This feature
            ;; requires chicken > 4.6.0
            (let loop ((egg-info-files
                        (glob (make-pathname tmp-repo-lib-dir "*.egg-info"))))
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
      (let ((data (read-egg-file egg (if this-egg? #f cache-dir))))
        (make-report egg 'meta-data (and data #t) data 0)))

    (define (check-egg-doc egg)
      (let ((start (current-seconds))
            (doc-exists?
             (egg-doc-exists? egg
                              eggs-doc-dir: eggs-doc-dir
                              major-version: (if eggs-doc-dir #f major-version)))
            (end (current-seconds)))
        (make-report egg 'check-doc (if doc-exists? 0 1) "" (- end start))))

    (define (check-dependencies egg meta-data)
      (let* ((egg-deps (get-egg-dependencies meta-data 'with-test-deps))
             (invalid-deps
              (filter (lambda (dep)
                        (chicken-unit? dep major-version))
                      egg-deps))
             (invalid-deps? (not (null? invalid-deps)))
             (non-symbol-deps (remove (lambda (egg) (symbol? egg)) egg-deps)))
        (make-report egg 'check-dependencies (not (or invalid-deps? (not (null? non-symbol-deps))))
                     (cond (invalid-deps?
                            (string-append
                             "The following chicken units are in one of the "
                             "dependencies list of this egg: "
                             (string-intersperse
                              (map ->string invalid-deps)
                              ", ")))
                           ((not (null? non-symbol-deps))
                            (string-append
                             "The following dependencies are not symbols: "
                             (string-intersperse
                              (map (lambda (dep)
                                     (with-output-to-string (cut pp dep)))
                                   non-symbol-deps)
                              ", ")))
                           (else ""))
                     0)))

    (define (check-category egg meta-data)
      (let* ((valid-categories
              '(lang-exts graphics debugging logic net io db os ffi web xml
                doc-tools egg-tools math oop data parsing tools sound testing
                crypt ui code-generation macros misc hell uncategorized obsolete))
             (egg-category (and-let* ((categ (alist-ref 'category meta-data)))
                             (car categ)))
             (valid-category? (and (symbol? egg-category)
                                   (memq egg-category valid-categories)
                                   #t)))
        (make-report egg 'check-category valid-category?
                     (if valid-category?
                         ""
                         (cond ((not egg-category)
                                "The `category' field has not been specified")
                               ((not (symbol? egg-category))
                                "The specified category is not a symbol")
                               ((not (memq egg-category valid-categories))
                                (conc "The specified category is invalid: "
                                      egg-category))))
                     0)))

    (define (check-license egg meta-data)
      (let ((license (alist-ref 'license meta-data)))
        (make-report egg
                     'check-license
                     (and license #t)
                     (if license
                         ""
                         "Missing license information")
                     0)))

    (define (check-author egg meta-data)
      (let ((author (alist-ref 'author meta-data)))
        (make-report egg
                     'check-author
                     (and author #t)
                     (if author
                         ""
                         "Missing author information")
                     0)))

    (define (clear-repo! egg)
      (when (file-exists? tmp-repo-dir)
        (for-each delete-path
                  (glob (make-pathname tmp-repo-dir "*"))))
      (delete-path (make-pathname tmp-dir egg)))

    (define (clear-chicken-home!)
      (let ((chicken-home
             (shell-command-output
              csi
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
                          (shell-command-output csc '(-cc-name))))
             (c++-compiler (strip-surrounding-quotes
                            (shell-command-output csc '(-cxx-name))))
             (c-compiler-version (program-version c-compiler))
             (c++-compiler-version (program-version c++-compiler)))
        #<#EOF
salmonella #salmonella-version -- a tool for testing CHICKEN eggs (http://wiki.call-cc.org/egg/salmonella)

Started on #(seconds->string (current-seconds))
Command line: #(string-intersperse (argv))

Options:
  chicken-install: #chicken-install
  repo-dir: #tmp-repo-dir
  chicken-install-args: #(chicken-install-args tmp-repo-dir)

C compiler: #c-compiler
#c-compiler-version

C++ compiler: #c++-compiler
#c++-compiler-version

C compiler flags: #(shell-command-output csc '(-cflags))

Linker: #(strip-surrounding-quotes (shell-command-output csc '(-ld-name)))
Linker flags: #(shell-command-output csc '(-ldflags))

Libraries: #(shell-command-output csc '(-libs))

CHICKEN banner:
#(shell-command-output csi '(-version))
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

        ((fetch) (fetch-egg egg))

        ((install) (install-egg egg))

        ((test) (test-egg egg))

        ((check-version) (check-version egg))

        ((env-info) (env-info))

        ((meta-data) (meta-data egg))

        ((check-dependencies) (check-dependencies egg (car more-args)))

        ((check-category) (check-category egg (car more-args)))

        ((check-doc) (check-egg-doc egg))

        ((check-license) (check-license egg (car more-args)))

        ((check-author) (check-author egg (car more-args)))

        (else (error 'salmonella "Invalid action" action))))))

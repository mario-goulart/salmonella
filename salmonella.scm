(module salmonella

(;; Exported API
 make-salmonella log! delete-path

 ;; report record
 make-report report->list report?
 report-egg report-egg-set!
 report-action report-action-set!
 report-status report-status-set!
 report-message report-message-set!
 report-duration report-duration-set!
 )

(import scheme chicken irregex)
(use srfi-1 srfi-13 posix setup-download setup-api tcp data-structures
     ports extras files utils)

(include "salmonella-common.scm")

(define-record report egg action status message duration)

(define (report->list report)
  (list (report-egg report)
        (report-action report)
        (report-status report)
        (report-message report)
        (report-duration report)))

(define (format-command command)
  (string-append
   (string-intersperse
    (map qs (map ->string command)))
   " 2>&1"))

(define (run-shell-command command #!optional (omit-command #f))
  ;; Returns (values <status> <output> <duration>)
  (let* ((command (format-command command))
         (start (current-seconds))
         (p (open-input-pipe command))
         (output (read-all p)))
    (values (arithmetic-shift (close-input-pipe p) -8)
            (conc (if omit-command "" (conc command "\n")) output)
            (- (current-seconds) start))))

(define (shell-command-output command)
  (let-values (((status output _) (run-shell-command command 'ommit-command)))
    (unless (zero? status)
      (error 'shell-command-output
             (sprintf "Command '~a' exited status ~a. Output:\n~a"
                      (format-command command)
                      status
                      output)))
    (string-chomp output)))

(define (save-excursion dir proc)
  (let ((current-dir (current-directory)))
    (change-directory dir)
    (let ((out (proc)))
      (change-directory current-dir)
      out)))


(define (delete-path . paths)
  (for-each (lambda (path)
              (when (file-exists? path)
                (if (directory? path)
                    (delete-directory path 'recursive)
                    (delete-file path))))
            paths))

(define (log! report log-file)
  (with-output-to-file log-file
    (lambda ()
      (pp (report->list report)))
    append:))

(define (chicken-unit? lib)
  (and (memq lib '(library eval expand data-strucutures ports files
                   extras irregex srfi-1 srfi-4 srfi-13 srfi-14
                   srfi-18 srfi-69 posix utils tcp lolevel foreign))
       #t))

(define (egg-installed? egg repo-lib-dir)
  (let ((installed-eggs
        (map pathname-file
             (glob (make-pathname repo-lib-dir "*" "setup-info")))))
    (and (member (->string egg) installed-eggs) #t)))

;;; meta data
(define (read-meta-file egg tmp-repo-dir)
  ;; If `tmp-repo-dir' is `#f', assume this-egg
  (let* ((egg (symbol->string egg))
         (meta-file (make-pathname (and tmp-repo-dir (list tmp-repo-dir egg))
                                   egg
                                   "meta")))
    (and (file-exists? meta-file)
         (handle-exceptions exn
           #f
           (with-input-from-file meta-file read)))))


;;; HTTP (for docs)

(define egg-doc-host "wiki.call-cc.org")
(define egg-doc-port 80)

(define (HEAD-request location)
  (conc "HEAD "
        location
        " HTTP/1.1" "\r\n"
        "Connection: close\r\n"
        "User-Agent: salmonella\r\n"
        "Accept: */*\r\n"
        "Host: " egg-doc-host #\: egg-doc-port "\r\n"
        "Content-length: 0\r\n"
        "\r\n"))

;; Stolen from setup-download.scm
(define (match-http-response rsp)
  (and (string? rsp)
       (irregex-match "HTTP/[0-9.]+\\s+([0-9]+)\\s+.*" rsp)) )

(define (response-match-code? mrsp code)
  (and mrsp (string=? (number->string code) (irregex-match-substring mrsp 1))) )

(define (egg-doc-exists? egg #!key eggs-doc-dir major-version)
  (cond (eggs-doc-dir
         (file-exists? (make-pathname eggs-doc-dir (->string egg))))
        (major-version
         (handle-exceptions enx
           #f
           (let-values (((in out) (tcp-connect egg-doc-host egg-doc-port)))
             (let ((req (HEAD-request
                         (make-absolute-pathname
                          `("eggref" ,(number->string major-version))
                          (->string egg)))))
               (display req out)
               (flush-output out)
               (let ((doc-exists? (response-match-code? (match-http-response (read-line in)) 200)))
                 (close-output-port out)
                 (close-input-port in)
                 doc-exists?)))))
        (else (error 'egg-doc-exists?
                     "Missing one of `major-version' or `eggs-doc-dir'"))))


;;; Salmonella

(define (make-salmonella tmp-dir
         #!key chicken-installation-prefix
               chicken-install-args
               eggs-source-dir
               eggs-doc-dir
               this-egg?)

  (let* ((mingw? (eq? (build-platform) 'mingw32))
         (chicken-installation-prefix (or chicken-installation-prefix (installation-prefix)))
         (eggs-source-dir
          (and eggs-source-dir
               (if (absolute-pathname? eggs-source-dir)
                   eggs-source-dir
                   (normalize-pathname
                    (make-pathname (current-directory) eggs-source-dir)))))
         (chicken-install-args
          (or chicken-install-args
              (lambda (repo-dir)
                `(-prefix ,repo-dir
                          ,@(if eggs-source-dir
                                `(-t local -l ,eggs-source-dir)
                                '(-test))))))
         (chicken-install
          (make-pathname (list chicken-installation-prefix "bin")
                         "chicken-install"
                         (and mingw? "exe")))
        (csi (make-pathname (list chicken-installation-prefix "bin")
                            "csi"
                            (and mingw? "exe")))
        (tmp-repo-dir (make-pathname tmp-dir "repo"))
        (binary-version (shell-command-output `(,csi -p "(##sys#fudge 42)")))
        (major-version
         (string->number
          (shell-command-output `(,csi -p "(car (string-split (chicken-version) \".\"))"))))
        (lib-dir (make-pathname '("lib" "chicken") binary-version))
        (tmp-repo-lib-dir (make-pathname tmp-repo-dir lib-dir))
        (egg-information (if eggs-source-dir
                             (gather-egg-information eggs-source-dir)
                             '())))

    (for-each (lambda (file)
                (unless (and (file-exists? file)
                             (file-execute-access? file))
                  (error 'make-salmonella
                         (conc file " cannot be found or have no execute access."))))
              (list chicken-install csi))

    ;; Set environment variables (CHICKEN_REPOSITORY will only be set
    ;; after initializing the repository)
    (setenv "SALMONELLA_RUNNING" "1")
    (setenv "CHICKEN_INSTALL_PREFIX" tmp-repo-dir)
    (setenv "CHICKEN_INCLUDE_PATH" (make-pathname tmp-repo-dir "share/chicken"))
    (setenv "CHICKEN_C_INCLUDE_PATH" (make-pathname tmp-repo-dir "include/chicken"))
    (setenv "PATH" (string-intersperse
                   (list (make-pathname tmp-repo-dir "bin")
                         (make-pathname chicken-installation-prefix "bin")
                         (get-environment-variable "PATH"))
                    (if (eq? (software-type) 'windows)
                        ";"
                        ":")))

    (define (log-shell-command egg action command)
      (let-values (((status output duration) (run-shell-command command)))
        (make-report egg action status output duration)))

    (define (fetch-egg egg #!key (action 'fetch) version)
      ;; Fetches egg and returns a report object
      (save-excursion tmp-dir
        (lambda ()
          (if (and this-egg? (eq? action 'fetch)) ;; don't fetch this egg
              (make-report egg 'fetch 0 "" 0)
              (log-shell-command egg
                                 'fetch
                                 `(,chicken-install
                                   -r ,@(chicken-install-args tmp-repo-dir)
                                   ,(if version
                                        (conc egg ":" version)
                                        egg)))))))

    (define (install-egg egg #!optional (action 'install))
      ;; Installs egg and returns a report object
      (let ((install
             (lambda ()
               (log-shell-command
                egg
                'install
                `(,chicken-install
                  ,@(delete '-test (chicken-install-args tmp-repo-dir)))))))
        (if (and this-egg? (eq? action 'install)) ;; install this egg from this dir
            (install)
            (save-excursion (make-pathname tmp-dir (->string egg)) install))))


    (define (test-egg egg)
      ;; Runs egg tests and returns a report object
      (let ((start (current-seconds)))
        ;; Installing test dependencies
        (let* ((meta-data (read-meta-file egg (if this-egg? #f tmp-dir)))
               (test-deps (alist-ref 'test-depends meta-data)))
          (for-each
           (lambda (dep)
             (unless (egg-installed? dep tmp-repo-lib-dir)
               (let* ((egg (if (pair? dep)
                               (car dep)
                               dep))
                      (version (and (list? dep) (cadr dep)))
                      (fetch-log (fetch-egg egg action: 'fetch-test-dep version: version)))
                 (when (and (zero? (report-status fetch-log))
                            (directory-exists? ;; workaround for issue with chicken 4.5.0 and regex
                             (make-pathname tmp-dir (->string egg))))
                   (install-egg egg 'install-test-dep)))))
           (remove chicken-unit? (or test-deps '()))))
        (let ((test-dir (make-pathname (if this-egg?
                                           #f
                                           (list tmp-dir (->string egg)))
                                       "tests")))
          (if (and (directory? test-dir)
                   (file-exists? (make-pathname test-dir "run.scm")))
              (save-excursion test-dir
                (lambda ()
                  (let ((report
                         (log-shell-command
                          egg
                          'test
                          `(,csi -script run.scm
                                 ,(if (eq? (software-type) 'windows)
                                      ""
                                      "< /dev/null")))))
                    (report-duration-set! report (- (current-seconds) start))
                    report)))
              (make-report egg 'test -1 "" 0)))))


    (define (check-version egg)
      ;; Check egg version and return a report object
      (let ((installed-version
             (and-let* ((version
                         (shell-command-output
                          `(,csi -e ,(sprintf "\"(print (extension-information '~a))\"" egg))))
                        (version (alist-ref 'version version)))
               (->string (car version)))))
        (if eggs-source-dir
          (let* ((setup-version
                  (and-let* ((egg-info (alist-ref egg egg-information))
                             (ver (alist-ref 'version egg-info)))
                    (->string (car ver))))
                 (version-ok? (equal? installed-version setup-version)))
            (make-report egg
                         'check-version
                         (if version-ok? 0 1)
                         (if version-ok?
                             ""
                             (conc "Mismatch between installed egg version "
                                   installed-version
                                   " and declared egg version "
                                   setup-version))
                         installed-version))
          (make-report egg
                       'check-version
                       -1
                       "Version check requires a local repository of eggs sources."
                       installed-version))))

    (define (meta-data egg)
      (let ((data (read-meta-file egg (if this-egg? #f tmp-dir))))
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
              (filter chicken-unit? egg-deps))
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
                         (cond ((not (symbol? egg-category))
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

    (define (env-info)
      (define (show-envvar var #!optional value)
        (string-append "  " var ": "
                       (or value
                           (cond ((get-environment-variable var)
                                  => (lambda (val)
                                       val))
                                 (else "(not set)")))))
      #<#EOF
salmonella -- a tool for testing Chicken eggs (http://wiki.call-cc.org/egg/salmonella)

Started on #(seconds->string (current-seconds))
Command line: #(string-intersperse (argv))

Options:
  chicken-install: #chicken-install
  repo-dir: #tmp-repo-dir
  chicken-install-args: #(chicken-install-args tmp-repo-dir)

Chicken banner:
#(shell-command-output `(,csi -version))
Environment variables:
#(show-envvar "SALMONELLA_RUNNING")
#(show-envvar "CHICKEN_INSTALL_PREFIX")
#(show-envvar "CHICKEN_INCLUDE_PATH")
#(show-envvar "CHICKEN_C_INCLUDE_PATH")
#(show-envvar "CHICKEN_REPOSITORY" tmp-repo-lib-dir)
#(show-envvar "CHICKEN_HOME")
#(show-envvar "CSC_OPTIONS")
#(show-envvar "PATH")

EOF
) ;; Beware of the hack above.  CHICKEN_REPOSITORY is only set by
  ;; salmonella after `init-repo!' is called.  Here we print its value
  ;; but the environment variable may not be actually set, since
  ;; `env-info' can be called before `init-repo!'.


    (lambda (action #!optional egg #!rest more-args)

      (case action
        ((clear-repo!) (begin
                         (when (file-exists? tmp-repo-dir)
                           (for-each delete-path
                                     (glob (make-pathname tmp-repo-dir "*"))))
                         (delete-path (make-pathname tmp-dir egg))))

        ((init-repo!) (begin
                        (parameterize ((setup-verbose-mode #f)
                                       (run-verbose #f))
                          (create-directory/parents tmp-repo-lib-dir))
                        (unsetenv "CHICKEN_REPOSITORY")
                        (receive (_ _ _) ;; make the scrutinizer happy
                            (run-shell-command
                             `(,chicken-install -init ,tmp-repo-lib-dir))
                          (void))
                        ;; Only set CHICKEN_REPOSITORY after initializing the repo
                        (setenv "CHICKEN_REPOSITORY" tmp-repo-lib-dir)))

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

) ; end module

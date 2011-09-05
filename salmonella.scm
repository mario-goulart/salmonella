(use srfi-13 posix setup-download setup-api tcp irregex)

(define-record report egg action status message duration)

(define (run-shell-command command #!optional (omit-command #f))
  ;; Returns (values <status> <output> <duration>)
  (let* ((start (current-seconds))
         (p (open-input-pipe
             (string-append "SALMONELLA_RUNNING=1 " command " 2>&1")))
         (output (read-all p)))
    (values (arithmetic-shift (close-input-pipe p) -8)
            (conc (if omit-command "" (conc command "\n")) output)
            (- (current-seconds) start))))


(define (save-excursion dir proc)
  (let ((current-dir (current-directory)))
    (change-directory dir)
    (let ((out (proc)))
      (change-directory current-dir)
      out)))


(define (delete-path . paths)
  ;; mostly stolen from chicken-setup.scm
  (define *windows*
    (and (eq? (software-type) 'windows)
         (build-platform) ) )

  (define *windows-shell* (memq *windows* '(msvc mingw32)))

  (let ((cmd (if *windows-shell* "del /Q /S" "rm -fr")))
    (for-each (lambda (path) (system* "~a ~a" cmd path)) paths)))


(define (log! report log-file)
  (with-output-to-file log-file
    (lambda ()
      (pp (list (report-egg report)
                (report-action report)
                (report-status report)
                (report-message report)
                (report-duration report))))
    append:))


;;; meta data
(define (read-meta-file egg tmp-repo-dir)
  ;; If `tmp-repo-dir' is `#f', assume this-egg
  (let* ((egg (symbol->string egg))
         (meta-file (make-pathname (and tmp-repo-dir (list tmp-repo-dir egg))
                                   egg
                                   "meta")))
    (and (file-exists? meta-file)
         (with-input-from-file meta-file read))))


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
         (let-values (((in out) (tcp-connect egg-doc-host egg-doc-port)))
           (let ((req (HEAD-request
                       (make-absolute-pathname
                        `("eggref" ,(number->string major-version))
                        (->string egg)))))
             (display req out)
             (flush-output out)
             (response-match-code? (match-http-response (read-line in)) 200))))
        (else (error 'egg-doc-exists?
                     "Missing one of `major-version' or `eggs-doc-dir'"))))


;;; Salmonella

(define (make-salmonella tmp-dir
         #!key chicken-installation-prefix
               chicken-install-args
               eggs-source-dir
               eggs-doc-dir
               this-egg?)

  (let* ((chicken-installation-prefix (or chicken-installation-prefix (installation-prefix)))
         (chicken-install-args
          (or chicken-install-args
              (lambda (repo-dir)
                (string-append " -prefix " repo-dir
                               (if eggs-source-dir
                                   (string-append " -t local -l " eggs-source-dir)
                                   " -test")))))
         (chicken-install
          (make-pathname (list chicken-installation-prefix "bin")
                         "chicken-install"))
        (csi (make-pathname (list chicken-installation-prefix "bin") "csi"))
        (tmp-repo-dir (make-pathname tmp-dir "repo"))
        (binary-version
         (call-with-input-pipe (string-append csi " -p '(##sys#fudge 42)'")
                               read-line))
        (major-version
         (string->number
          (call-with-input-pipe
           (string-append csi " -p '(car (string-split (chicken-version) \".\"))'")
           read-line)))
        (lib-dir (make-pathname '("lib" "chicken") binary-version))
        (tmp-repo-lib-dir (make-pathname tmp-repo-dir lib-dir))
        (chicken-env-vars
         (string-append
          "CHICKEN_INSTALL_PREFIX=" tmp-repo-dir
          " "
          "CHICKEN_INCLUDE_PATH=" (make-pathname tmp-repo-dir "share/chicken")
          " "
          "CHICKEN_REPOSITORY=" tmp-repo-lib-dir))
        (egg-information (if eggs-source-dir
                             (gather-egg-information eggs-source-dir)
                             '())))

    (define (log-shell-command egg action command)
      (let-values (((status output duration) (run-shell-command command)))
        (make-report egg action status output duration)))

    (define (fetch-egg egg #!optional (action 'fetch))
      ;; Fetches egg and returns a report object
      (save-excursion tmp-dir
        (lambda ()
          (if this-egg?
              (make-report egg 'fetch 0 "" 0)
              (log-shell-command egg
                                 'fetch
                                 (sprintf "~a -r ~a ~a"
                                          chicken-install
                                          (chicken-install-args tmp-repo-dir)
                                          egg))))))

    (define (install-egg egg #!optional (action 'install))
      ;; Installs egg and returns a report object
      (let ((install
             (lambda ()
               (log-shell-command egg
                                  'install
                                  (sprintf "~a ~a ~a ~a"
                                           chicken-env-vars
                                           chicken-install
                                           (chicken-install-args tmp-repo-dir)
                                           (if this-egg?
                                               ""
                                               egg))))))
        (if this-egg?
            (install)
            (save-excursion (make-pathname tmp-dir (->string egg)) install))))


    (define (test-egg egg)
      ;; Runs egg tests and returns a report object
      (let ((start (current-seconds)))
        ;; Installing test dependencies
        (let* ((meta-data (read-meta-file egg (if this-egg? #f tmp-dir)))
               (test-deps (alist-ref 'test-depends meta-data)))
          (for-each (lambda (dep)
                      (fetch-egg dep 'fetch-test-dep)
                      (install-egg dep 'install-test-dep))
                    (or test-deps '())))
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
                          (sprintf "~a ~a -script run.scm"
                                   chicken-env-vars csi))))
                    (report-duration-set! report (- (current-seconds) start))
                    report)))
              (make-report egg 'test -1 "" 0)))))


    (define (check-version egg)
      ;; Check egg version and return a report object
      (let ((installed-version
             (and-let* ((version
                         (with-input-from-pipe
                          (sprintf "~a ~a -e \"(print (extension-information '~a))\""
                                   chicken-env-vars csi egg)
                          read))
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
        (make-report egg 'doc (if doc-exists? 0 1) "" (- end start))))

    (define (check-dependencies egg meta-data)
      (define (deps key)
        (or (and-let* ((d (assq key meta-data)))
              (cdr d))
            '()))
      (let* ((chicken-units '(library eval expand data-strucutures ports files
                              extras irregex srfi-1 srfi-4 srfi-13 srfi-14
                              srfi-18 srfi-69 posix utils tcp lolevel foreign))
             (egg-deps (map (lambda (dep)
                              (if (pair? dep)
                                  (car dep)
                                  dep))
                            (append (deps 'depends)
                                    (deps 'needs)
                                    (deps 'test-depends))))
             (invalid-deps
              (filter (lambda (dep)
                        (memq dep chicken-units))
                      egg-deps))
             (invalid-deps? (null? invalid-deps)))
        (make-report egg 'check-dependencies invalid-deps?
                     (if invalid-deps?
                         (string-append
                          "The following chicken units are in one of the "
                          "dependencies list of this egg: "
                          (string-intersperse
                           (map ->string invalid-deps)
                           ", "))
                         "")
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


    (define (env-info)
      #<#EOF
salmonella -- a tool for testing Chicken eggs (http://wiki.call-cc.org/egg/salmonella)
Started on #(seconds->string (current-seconds))
Chicken version: #(nth-value 1 (run-shell-command (string-append chicken-install " -version") 'omit-cmd))
Options:
  chicken-install: #chicken-install
  tmp-repo-dir: #tmp-repo-dir
  chicken-install-args: #(chicken-install-args tmp-repo-dir)
  eggs-source-dir: #eggs-source-dir
EOF
)

    (lambda (action #!optional egg #!rest more-args)

      (case action
        ((clear-repo!) (begin
                         (delete-path (make-pathname tmp-repo-dir "*"))
                         (delete-path (make-pathname tmp-dir egg))))

        ((init-repo!) (begin
                        (create-directory tmp-repo-lib-dir 'parents)
                        (run-shell-command
                         (sprintf "~a -init ~a" chicken-install tmp-repo-lib-dir))))

        ((fetch) (fetch-egg egg))

        ((install) (install-egg egg))

        ((test) (test-egg egg))

        ((check-version) (check-version egg))

        ((env-info) (env-info))

        ((meta-data) (meta-data egg))

        ((check-dependencies) (check-dependencies egg (car more-args)))

        ((check-category) (check-category egg (car more-args)))

        ((doc) (check-egg-doc egg))

        (else (error 'salmonella "Invalid action" action))))))

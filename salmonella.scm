(use srfi-13 posix setup-download)

(define-record report egg action status message duration)

(define (run-shell-command command #!optional (omit-command #f))
  ;; Returns (values <status> <output> <duration>)
  (let* ((start (current-seconds))
         (p (open-input-pipe (string-append command " 2>&1")))
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
  (let ((egg (->string egg)))
    (with-input-from-file
        (make-pathname (list tmp-repo-dir egg) egg "meta")
      read)))


(define (make-salmonella tmp-dir
         #!key chicken-installation-prefix
               chicken-install-args
               eggs-source-dir)

  (let* ((chicken-installation-prefix (or chicken-installation-prefix "/usr"))
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
          (log-shell-command egg
                             'fetch
                             (sprintf "~a -r ~a ~a"
                                      chicken-install
                                      (chicken-install-args tmp-repo-dir)
                                      egg)))))

    (define (install-egg egg #!optional (action 'install))
      ;; Installs egg and returns a report object
      (save-excursion (make-pathname tmp-dir (->string egg))
        (lambda ()
          (log-shell-command egg
                             'install
                             (sprintf "~a ~a ~a ~a"
                                      chicken-env-vars
                                      chicken-install
                                      (chicken-install-args tmp-repo-dir)
                                      egg)))))

    (define (test-egg egg)
      ;; Runs egg tests and returns a report object
      (let ((start (current-seconds)))
        ;; Installing test dependencies
        (let* ((meta-data (read-meta-file egg tmp-dir))
               (test-deps (alist-ref 'test-depends meta-data)))
          (for-each (lambda (dep)
                      (fetch-egg dep 'fetch-test-dep)
                      (install-egg dep 'install-test-dep))
                    (or test-deps '())))
        (let ((test-dir (make-pathname (list tmp-dir (->string egg)) "tests")))
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
      (if eggs-source-dir
          (let* ((setup-version
                  (and-let* ((version
                              (with-input-from-pipe
                               (sprintf "~a ~a -e \"(print (extension-information '~a))\""
                                        chicken-env-vars csi egg)
                               read))
                             (version (alist-ref 'version version)))
                    (->string (car version))))
                 (tag-version
                  (and-let* ((egg-info (alist-ref egg egg-information))
                             (ver (alist-ref 'version egg-info)))
                    (->string (car ver))))
                 (version-ok? (equal? setup-version tag-version)))
            (make-report egg
                         'check-version
                         (if version-ok? 0 1)
                         (if version-ok?
                             ""
                             (conc "Mismatch between installed egg version "
                                   setup-version
                                   " and declared egg version "
                                   tag-version))
                         0))
          (error 'check-version
                 "Version check requires a local repository of eggs sources."
                 "See the `eggs-source-dir' keyword parameter for `make-salmonella.")))

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

    (lambda (action #!optional egg)

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

        (else (error 'salmonella "Invalid action" action))))))

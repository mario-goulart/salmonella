(module salmonella

(;; Exported API
 make-salmonella log!

 ;; report record
 make-report report->list report?
 report-egg report-egg-set!
 report-action report-action-set!
 report-status report-status-set!
 report-message report-message-set!
 report-duration report-duration-set!
 )

(import scheme)
(cond-expand
 (chicken-4
  (import chicken foreign)
  (use data-structures files irregex posix tcp utils)
  (import (except setup-api copy-file))
  (define file-executable? file-execute-access?)
  (define file-readable? file-read-access?)
  (define copy-file file-copy)
  (define set-file-permissions! change-file-mode))
 (chicken-5
  (import (chicken base)
          (chicken bitwise)
          (chicken condition)
          (chicken fixnum)
          (chicken format)
          (chicken io)
          (chicken platform)
          (chicken pretty-print)
          (chicken process)
          (chicken tcp)
          (chicken time)
          (chicken time posix))
  (include "libs/srfi-1.scm"))
 (else
  (error "Unsupported CHICKEN version.")))

(include "salmonella-common.scm")
(include "salmonella-version.scm")

(define-record report egg action status message duration)

(define (report->list report)
  (list (report-egg report)
        (report-action report)
        (report-status report)
        (report-message report)
        (report-duration report)))

(define default-csi-program (foreign-value "C_CSI_PROGRAM" c-string))

(define default-csc-program (foreign-value "C_CSC_PROGRAM" c-string))

(define default-chicken-install-program
  (foreign-value "C_CHICKEN_INSTALL_PROGRAM" c-string))

;; From setup-api
(define *windows-shell* (foreign-value "C_WINDOWS_SHELL" bool))

(define (format-command command args)
  (string-append
   (let ((cmd (qs (normalize-pathname command))))
     (if *windows-shell*
         (string-append "\"" cmd)
         cmd))
   " "
   (string-intersperse (map ->string args))
   " 2>&1"
   (if *windows-shell*
       "\""
       " < /dev/null")))

(define (run-shell-command command args #!key omit-command?)
  ;; Returns (values <status> <output> <duration>)
  ;;
  ;; `omit-command?' controls whether command should be displayed or
  ;; not in <output> (to show users what command was executed to
  ;; obtain that output).
  (let* ((command (format-command command args))
         (start (current-seconds))
         (p (open-input-pipe command))
         (output (cond-expand
                  (chicken-4
                   (read-all p))
                  (chicken-5
                   (with-input-from-port p read-string)))))
    (values (if *windows-shell*
                (close-input-pipe p)
                (arithmetic-shift (close-input-pipe p) -8))
            (conc (if omit-command? "" (conc command "\n")) output)
            (- (current-seconds) start))))

(define (shell-command-output command args)
  (let-values (((status output _) (run-shell-command command args omit-command?: #t)))
    (unless (zero? status)
      (error 'shell-command-output
             (sprintf "Command '~a' exited status ~a. Output:\n~a"
                      (format-command command args)
                      status
                      output)))
    (string-chomp output)))

(define (save-excursion dir proc)
  (let ((current-dir (current-directory)))
    (change-directory dir)
    (let ((out (proc)))
      (change-directory current-dir)
      out)))

(define (strip-surrounding-quotes text)
  (let ((len (string-length text)))
    (if (zero? len)
        ""
        (if (and (char=? (string-ref text 0) #\')
                 (char=? (string-ref text (fx- len 1)) #\'))
            (substring text 1 (fx- len 1))
            text))))

(define (log! report log-file)
  (with-output-to-file log-file
    (lambda ()
      (pp (report->list report)))
    append:))

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
         (handle-exceptions exn
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


;; Salmonella actions & helpers

(define (salmonella-env tmp-dir
                        chicken-installation-prefix
                        chicken-install-args
                        this-egg?
                        clear-chicken-home?
                        csi
                        csc
                        chicken-install)
  (let* ((mingw? (eq? (build-platform) 'mingw32))
         (maybe-exe (lambda (name)
                      (if mingw?
                          (sprintf "~a.exe" name)
                          name)))
         (user-provided-installation-prefix chicken-installation-prefix)
         (chicken-installation-prefix
          (cond-expand
           (chicken-4
            (or user-provided-installation-prefix (installation-prefix)))
           (chicken-5
            (or user-provided-installation-prefix
                default-installation-prefix))))
         (tmp-repo-dir (make-pathname tmp-dir "repo"))
         (csi
          (or csi
              (make-pathname (list chicken-installation-prefix "bin")
                             (if user-provided-installation-prefix
                                 (maybe-exe "csi")
                                 default-csi-program))))
         (csc
          (or csc
              (make-pathname (list chicken-installation-prefix "bin")
                             (if user-provided-installation-prefix
                                 (maybe-exe "csc")
                                 default-csc-program))))
         (chicken-install
          (or chicken-install
              (make-pathname (list chicken-installation-prefix "bin")
                             (if user-provided-installation-prefix
                                 (maybe-exe "chicken-install")
                                 default-chicken-install-program))))
         (host-repository-path
          (shell-command-output chicken-install '(-repository)))
         (binary-version (pathname-file host-repository-path))
         (major-version
          (let ((v (shell-command-output
                    csi
                    (cond-expand
                     (chicken-4
                      '(-np "\"(chicken-version)\""))
                     (chicken-5
                      '(-np "\"(begin (import chicken.platform) (chicken-version))\""))))))
            (string->number (car (string-split v ".")))))
         (lib-dir (make-pathname '("lib" "chicken") binary-version))
         (tmp-repo-bin-dir (make-pathname tmp-repo-dir "bin"))
         (tmp-repo-lib-dir (make-pathname tmp-repo-dir lib-dir))
         (tmp-repo-share-dir (make-pathname (list tmp-repo-dir "share") "chicken"))
         (chicken-install-args
          (or chicken-install-args
              (lambda (repo-dir)
                (cond-expand
                 (chicken-4 `(-debug -prefix ,repo-dir -test))
                 (chicken-5 '(-v -test))))))
         (cache-dir (make-pathname tmp-repo-dir "cache")))
    (lambda (var)
      (case var
        ((clear-chicken-home?) clear-chicken-home?)
        ((chicken-installation-prefix) chicken-installation-prefix)
        ((chicken-install) chicken-install)
        ((chicken-install-args) chicken-install-args)
        ((host-repository-path) host-repository-path)
        ((cache-dir)
         (cond-expand
          (chicken-4
           (error 'salmonella-env "cache-dir is not available for CHICKEN 4."))
          (chicken-5
           cache-dir)))
        ((csi) csi)
        ((csc) csc)
        ((tmp-repo-dir) tmp-repo-dir)
        ((binary-version) binary-version)
        ((lib-dir) lib-dir)
        ((tmp-dir) tmp-dir)
        ((tmp-repo-bin-dir) tmp-repo-bin-dir)
        ((tmp-repo-lib-dir) tmp-repo-lib-dir)
        ((tmp-repo-share-dir) tmp-repo-share-dir)
        ((major-version) major-version)
        ((this-egg?) this-egg?)
        (else (error 'salmonella-env "Unsupported attribute:" var))))))

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

(define (egg-installed? egg env)
  (let ((installed-eggs
         (map pathname-file
              (glob (make-pathname (env 'tmp-repo-lib-dir) "*"
                                   (cond-expand
                                    (chicken-4 "setup-info")
                                    (chicken-5 "egg-info")))))))
    (and (member (->string egg) installed-eggs) #t)))

(define (get-chicken-home env)
  (shell-command-output
   (env 'csi)
   (cond-expand
    (chicken-4
     '(-np "\"(chicken-home)\""))
    (chicken-5
     '(-np "\"(begin (import (chicken platform)) (chicken-home))\"")))))

(define (clear-repo! egg env)
  (when (file-exists? (env 'tmp-repo-dir))
    (for-each delete-path
              (glob (make-pathname (env 'tmp-repo-dir) "*"))))
  (delete-path (make-pathname (env 'tmp-dir) egg))

  ;; In CHICKEN 4, we clear chicken-home in clear-repo!.
  (cond-expand
   (chicken-4
    (when (env 'clear-chicken-home?)
      (clear-chicken-home! env)))
   (else)))

(define (clear-chicken-home! env)
  (for-each delete-path
            (glob (make-pathname (get-chicken-home env) "*.scm"))))

(define (init-tmp-repo-bin-dir! env)
  (create-directory (env 'tmp-repo-bin-dir) 'recursively)
  (for-each
   (lambda (file)
     (let ((dest-file (make-pathname (env 'tmp-repo-bin-dir)
                                     (symbol->string file))))
       (copy-file (env file) dest-file 'clobber)
       (set-file-permissions! dest-file (file-permissions (env file)))))
   '(csc csi)))

(define (read-meta-file egg env)
  ;; in CHICKEN 4, read .meta file; in CHICKEN 5, read .egg file
  (let* ((egg (symbol->string egg))
         (meta-file
          (cond-expand
           (chicken-4
            (make-pathname (if (env 'this-egg?)
                               #f
                               (list (env 'tmp-dir) egg))
                           egg
                           "meta"))
           (chicken-5
            (make-pathname (if (env 'this-egg?)
                               #f
                               (list (env 'cache-dir) egg))
                           egg
                           "egg")))))
    (and (file-readable? meta-file)
         (handle-exceptions exn
           #f
           (with-input-from-file meta-file read)))))

(define (check-chicken-executables env)
  (for-each (lambda (file)
              (unless (file-executable? (env file))
                (error 'check-chicken-executables
                       (conc file " cannot be found or have no execute access."))))
            '(chicken-install csc csi)))

(define (salmonella-system-path env)
  (string-intersperse
   (list (make-pathname (env 'tmp-repo-dir) "bin")
         (get-environment-variable "PATH"))
   (if (eq? (software-type) 'windows)
       ";"
       ":")))

(define (log-shell-command egg action command args)
  (let-values (((status output duration) (run-shell-command command args)))
    (make-report egg action status output duration)))

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

(define (check-dependencies egg meta-data major-version)
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

(define (check-egg-doc egg eggs-doc-dir major-version)
  (let ((start (current-seconds))
        (doc-exists?
         (egg-doc-exists? egg
                          eggs-doc-dir: eggs-doc-dir
                          major-version: (if eggs-doc-dir #f major-version)))
        (end (current-seconds)))
    (make-report egg 'check-doc (if doc-exists? 0 1) "" (- end start))))

(define (find-egg-info-files egg env)
  ;; In CHICKEN 4; find .setup-info files; in CHICKEN 5, find
  ;; .egg-info files
  (let* ((suffix (cond-expand
                  (chicken-4 "setup-info")
                  (chicken-5 "egg-info")))
         (egg-info-file (make-pathname (env 'tmp-repo-lib-dir) egg suffix)))
    (if (and (file-exists? egg-info-file)
             (file-readable? egg-info-file))
        (list egg-info-file)
        ;; extension installs more than one module. Find them based
        ;; on the egg-name key in egg-info files. This feature
        ;; requires chicken > 4.6.0
        (let loop ((egg-info-files
                    (glob (make-pathname (env 'tmp-repo-lib-dir)
                                         (string-append "*." suffix)))))
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

;; FIXME: do still need this for CHICKEN 5?  Look for egg-info and
;; check if we can use egg-info instead.
(define (egg-info-version egg env)

  (define (read-version egg-info-file)
    (let ((data (with-input-from-file egg-info-file read)))
      (and-let* ((version (alist-ref 'version data)))
        (car version))))

  (let ((egg-info-files (find-egg-info-files egg env)))
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

(define (check-version egg env)
  ;; Check egg version and return a report object.
  ;; Status:
  ;;   0: success
  ;;   1: failure
  ;;  -1: ignore (cannot determine failure or success)
  (let ((installed-version (egg-info-version (symbol->string egg) env)))
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

(define (meta-data egg env)
  (let ((data (read-meta-file egg env)))
    (make-report egg 'meta-data (and data #t) data 0)))

(define (fetch-egg egg env #!key (action 'fetch) version)
  ;; Fetches egg and returns a report object
  (save-excursion (env 'tmp-dir)
    (lambda ()
      (if (and (env 'this-egg?) (eq? action 'fetch)) ;; don't fetch this egg
          (make-report egg 'fetch 0 "" 0)
          (log-shell-command egg
                             action
                             (env 'chicken-install)
                             `(-r ,@((env 'chicken-install-args) (env 'tmp-repo-dir))
                                  ,(if version
                                       (conc egg ":" version)
                                       egg)))))))

(define (install-egg egg env #!optional (action 'install))
  ;; Installs egg and returns a report object
  (let ((install
         (lambda ()
           (log-shell-command
            egg
            action
            (env 'chicken-install)
            `(,@(delete '-test ((env 'chicken-install-args) (env 'tmp-repo-dir))))))))
    (if (and (env 'this-egg?) (eq? action 'install)) ;; install this egg from this dir
        (install)
        (save-excursion (make-pathname
                         (cond-expand
                          (chicken-4 (env 'tmp-dir))
                          (chicken-5 (env 'cache-dir)))
                         (->string egg))
             install))))

(define (test-egg egg env)
  ;; Run egg tests and return a list of report objects.
  (let* ((start (current-seconds))
         (all-reports '())
         (test-dir (make-pathname (if (env 'this-egg?)
                                      #f
                                      (list (cond-expand
                                             (chicken-4 (env 'tmp-dir))
                                             (chicken-5 (env 'cache-dir)))
                                            (->string egg)))
                                  "tests"))
         (test-script (make-pathname test-dir "run.scm"))
         (add-to-reports!
          (lambda (report)
            (set! all-reports (cons report all-reports)))))
    (call/cc
     (lambda (return)
       (let* ((meta-data (read-meta-file egg env))
              (test-deps (or (alist-ref
                              (cond-expand
                                (chicken-4 'test-depends)
                                (chicken-5 'test-dependencies))
                              meta-data)
                             '())))
         (if (and (not (null? test-deps))
                  (not (file-exists? test-script)))
             (add-to-reports!
              (make-report egg 'useless-test-dependencies #f
                           "Test dependencies have been specified, but the egg has no tests."
                           0))
             ;; Install test dependencies
             (let loop ((deps (remove (lambda (dep)
                                        (chicken-unit? dep (env 'major-version)))
                                      test-deps)))
               (unless (null? deps)
                 (let* ((dep-maybe-version (car deps))
                        (dep (if (pair? dep-maybe-version)
                                 (car dep-maybe-version)
                                 dep-maybe-version)))
                   (if (egg-installed? dep env)
                       (loop (cdr deps))
                       (let* ((dep (if (pair? dep)
                                       (car dep)
                                       dep))
                              (version (and (pair? dep) (cadr dep)))
                              (fetch-log (fetch-egg dep env
                                                    action: `(fetch-test-dep ,egg)
                                                    version: version))
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
                                (return (reverse all-reports)))))))))))
       ;; At this point, fetching and installing test dependencies
       ;; succeeded, so proceed to run tests.
       (cond ((and (file-exists? test-script)
                   (file-readable? test-script))
              (save-excursion test-dir
                (lambda ()
                  (let ((report
                         (log-shell-command
                          egg
                          'test
                          (env 'csi)
                          `(-script run.scm ,(->string egg)))))
                    (report-duration-set! report (- (current-seconds) start))
                    (add-to-reports! report)
                    (reverse all-reports)))))
             (else
              (add-to-reports! (make-report egg 'test -1 "" 0))
              (reverse all-reports)))))))

(define (env-info env)
  (define (show-envvar var #!optional value)
    (string-append "  " var ": "
                   (or value
                       (cond ((get-environment-variable var)
                              => (lambda (val)
                                   val))
                             (else "(not set)")))))

  (define (program-version prog)
    ;; Try to obtain the program version by calling it with --version.
    ;; In case the program doesn't recognize that option, try -version
    ;; (e.g., TCC).  If both fail, return #f.
    (let-values (((status output dur)
                  (run-shell-command prog '(--version) omit-command?: #t)))
      (if (zero? status)
          output
          (let-values (((status output dur)
                        (run-shell-command prog '(-version) omit-command?: #t)))
            (and (zero? status) output)))))

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
#(string-intersperse
  (cond-expand
   (chicken-4
    (list
     (show-envvar "SALMONELLA_RUNNING")
     (show-envvar "CHICKEN_PREFIX" (env 'chicken-installation-prefix))
     (show-envvar "CHICKEN_INSTALL_PREFIX")
     (show-envvar "CHICKEN_INCLUDE_PATH")
     (show-envvar "CHICKEN_C_INCLUDE_PATH")
     (show-envvar "CHICKEN_REPOSITORY" (env 'tmp-repo-lib-dir))
     (show-envvar "CHICKEN_HOME")
     (show-envvar "CSC_OPTIONS")
     (show-envvar "PATH")))
  (chicken-5
   (list
    (show-envvar "SALMONELLA_RUNNING")
    (show-envvar "CHICKEN_INSTALL_REPOSITORY")
    (show-envvar "CHICKEN_REPOSITORY_PATH")
    (show-envvar "CHICKEN_EGG_CACHE")
    (show-envvar "CHICKEN_INCLUDE_PATH")
    (show-envvar "CHICKEN_C_INCLUDE_PATH")
    (show-envvar "CHICKEN_HOME")
    (show-envvar "CSC_OPTIONS")
    (show-envvar "PATH"))))
  "\n")

EOF
)) ;; Beware of the hack above.  In CHICKEN 4, CHICKEN_REPOSITORY and
   ;; CHICKEN_PREFIX are only set by salmonella after `init-repo!' is
   ;; called.  Here we print their value but the environment variable
   ;; may not be actually set, since `env-info' can be called before
   ;; `init-repo!'.



(import scheme)
(cond-expand
 (chicken-4
  (include "salmonella-4.scm"))
 (chicken-5
  (include "salmonella-5.scm"))
 (else
  (error "Unsupported CHICKEN version.")))

) ; end module

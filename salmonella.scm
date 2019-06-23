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
  (use data-structures irregex tcp utils))
 (chicken-5
  (import (chicken base)
          (chicken bitwise)
          (chicken condition)
          (chicken fixnum)
          (chicken format)
          (chicken io)
          (chicken pretty-print)
          (chicken process)
          (chicken tcp)
          (chicken time)))
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
       "")))

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


(import scheme)
(cond-expand
 (chicken-4
  (include "salmonella-4.scm"))
 (chicken-5
  (include "salmonella-5.scm"))
 (else
  (error "Unsupported CHICKEN version.")))

) ; end module

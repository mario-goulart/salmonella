(module salmonella-log-parser

(;; Exported API
 read-log-file log-eggs log-skipped-eggs status-zero?

 ;; fetch
 fetch-status fetch-message fetch-duration

 ;; install
 install-status install-message install-duration

 ;; check-version
 check-version-status check-version-message egg-version check-version-ok?

 ;; test
 test-status test-message test-duration has-test?

 ;; meta-data
 meta-data egg-dependencies egg-license

 ;; doc
 doc-exists?

 ;; start & end
 start-time salmonella-info end-time total-time

 ;; statistics
 count-install-ok count-install-fail count-test-ok count-test-fail
 count-no-test count-total-eggs count-documented count-undocumented

 ;; misc
 prettify-time sort-eggs log-version

 ;; low level stuff
 log-get
 )

(import scheme chicken)
(import (chicken base)
        (chicken io)
        (chicken file)
        (chicken format)
        (chicken process-context)
        (chicken random)
        (chicken sort)
        (chicken string))
(import salmonella)

(include "libs/srfi-1.scm")
(include "salmonella-common.scm")

(define (status-zero? status)
  (and status (zero? status)))

(define (get-by-egg/action egg action log)
  (find (lambda (entry)
          (and (eq? (report-egg entry) egg)
               (eq? (report-action entry) action)))
        log))

(define (log-version-0? log)
  ;; Log files emitted by salmonella 1.x had salmonella-info as a
  ;; string as the first entry
  (string? (car log)))

(define (read-log-file log-file)
  (let ((entries (with-input-from-file log-file read-list)))
    ;; Ugly hack to avoid breaking on old log files. We don't actually
    ;; support parsing old logs at the moment -- just avoid crashing.
    (if (log-version-0? entries)
        entries
        (map (lambda (entry)
               (apply make-report entry))
             entries))))

(define (log-get egg action getter log)
  (and-let* ((log-line (get-by-egg/action egg action log)))
    (getter log-line)))

(define (log-eggs log)
  ;; Return a list of eggs from `log', except skipped eggs
  (let loop ((log log)
             (eggs '()))
    (if (null? log)
        eggs
        (let* ((report (car log))
               (egg (report-egg report))
               (action (report-action report)))
          (loop (cdr log)
                (if (or (not (symbol? egg))
                        (memq egg eggs)
                        (eq? action 'skip))
                    eggs
                    (cons egg eggs)))))))


(define (log-skipped-eggs log)
  ;; Return a list of skipped eggs from `log'
  (let loop ((log log)
             (eggs '()))
    (if (null? log)
        eggs
        (let* ((report (car log))
               (egg (report-egg report))
               (action (report-action report)))
          (loop (cdr log)
                (if (and (symbol? egg)
                         (not (memq egg eggs))
                         (eq? action 'skip))
                    (cons egg eggs)
                    eggs))))))


;; fetch
(define (fetch-status egg log) (log-get egg 'fetch report-status log))
(define (fetch-message egg log) (log-get egg 'fetch report-message log))
(define (fetch-duration egg log) (log-get egg 'fetch report-duration log))


;; install
(define (install-status egg log) (log-get egg 'install report-status log))
(define (install-message egg log) (log-get egg 'install report-message log))
(define (install-duration egg log) (log-get egg 'install report-duration log))


;; check-version
(define (check-version-status egg log) (log-get egg 'check-version report-status log))
(define (check-version-message egg log) (log-get egg 'check-version report-message log))
(define (egg-version egg log) (log-get egg 'check-version report-duration log))

(define (check-version-ok? egg log)
  (let ((status (check-version-status egg log)))
    (or (status-zero? status) (= status -1))))


;; test
(define (test-status egg log) (log-get egg 'test report-status log))
(define (test-message egg log) (log-get egg 'test report-message log))
(define (test-duration egg log) (log-get egg 'test report-duration log))
(define (has-test? egg log)
  (let ((status (test-status egg log)))
    (and status (not (= status -1)))))

;; meta-data
(define (meta-data egg log) (log-get egg 'meta-data report-message log))

(define (egg-dependencies egg log #!key with-test-dependencies? with-versions?)
  ;; Make sure to call this procedure giving proper eggs as arguments.
  ;; Core libraries, for example, don't have metadata (.meta) and will
  ;; make egg-dependencies raise an error.
  (let ((data (meta-data egg log)))
    (if data
        (get-egg-dependencies data
                              with-test-dependencies?: with-test-dependencies?
                              with-versions?: with-versions?)
        (error 'egg-dependencies
               (sprintf "No metadata for ~a" egg)))))

(define (egg-license egg log)
  (let ((data (meta-data egg log)))
    (and-let* ((data)
               (license (alist-ref 'license data)))
      (car license))))

;; doc
(define (doc-exists? egg log)
  (status-zero? (log-get egg 'check-doc report-status log)))

;; log version

;; Version 0 (emitted by salmonella 1.x)
;;
;; Version 1 (emitted by salmonellas 2.0 - 2.7)
;;
;; Version 2 (emitted by salmonellas 2.8 - <current version>):
;;   * same format as log version 1's, but with version information
;;     -- `log-version' action.
;;
;; Version 3 (emitted by salmonellas 2.8 - <current version>):
;;   * test-egg produces multiple reports, as it fetches and installs
;;     test dependencies, besides testing eggs.  Actions for fetch and
;;     install of test dependencies are lists whose first element is
;;     either fetch-test-dep or install-test-dep and the second
;;     element is the egg being tested (not the dependency!).

(define (log-version log)
  (if (log-version-0? log)
      0
      (let loop ((log log))
        (if (null? log)
            1
            (let ((report (car log)))
              (if (eq? 'log-version (report-action report))
                  (report-message report)
                  (loop (cdr log))))))))

;; start & end
(define (start-report log)
  (let loop ((log log))
    (if (null? log)
        (error 'start-report "Could not determine start report entry.")
        (let ((current-report (car log)))
          (if (eq? 'start (report-action current-report))
              current-report
              (loop (cdr log)))))))

(define (start-time log)
  (report-duration (start-report log)))

(define (salmonella-info log)
  (report-message (start-report log)))

(define (end-time log)
  (report-duration (last log)))

(define (total-time log)
  (- (end-time log) (start-time log)))

;; statistics
(define (count-install-ok log)
  (count (lambda (egg)
           (status-zero? (install-status egg log)))
         (log-eggs log)))

(define (count-install-fail log)
  (- (count-total-eggs log) (count-install-ok log)))

(define (count-test-ok log)
  (count (lambda (entry)
           (and (eq? 'test (report-action entry))
                (status-zero? (report-status entry))))
         log))

(define (count-test-fail log)
  (count (lambda (entry)
           (and (eq? 'test (report-action entry))
                (> (report-status entry) 0)))
         log))

(define (count-no-test log)
  (count (lambda (entry)
           (and (eq? 'test (report-action entry))
                (< (report-status entry) 0)))
         log))

(define (count-total-eggs log #!key with-skipped?)
  (+ (length (log-eggs log))
     (if with-skipped?
         (length (log-skipped-eggs log))
         0)))

(define (count-documented log)
  (count (lambda (entry)
           (and (eq? 'check-doc (report-action entry))
                (status-zero? (report-status entry))))
         log))

(define (count-undocumented log)
  (count (lambda (entry)
           (and (eq? 'check-doc (report-action entry))
                (not (status-zero? (report-status entry)))))
         log))

;; Misc
(define (prettify-time seconds)
  (define (pretty-time seconds)
    (cond ((zero? seconds)
           "")
          ((< seconds 60)
           (conc seconds "s"))
          ((< seconds 3600)
           (let ((mins (quotient seconds 60)))
             (conc mins "m" (pretty-time (- seconds (* 60 mins))))))
          (else
           (let ((hours (quotient seconds 3600)))
             (conc hours "h" (pretty-time (- seconds (* 3600 hours))))))))
  (if (zero? seconds)
      "0s"
      (let ((pretty (pretty-time (abs (inexact->exact seconds)))))
        (if (negative? seconds)
            (string-append "-" pretty)
            pretty))))


(define (sort-eggs eggs)
  (sort eggs (lambda (e1 e2)
               (string<? (symbol->string e1)
                         (symbol->string e2)))))

) ;; end module

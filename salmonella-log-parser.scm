(module salmonella-log-parser

(;; Exported API
 read-log-file log-eggs log-skipped-eggs

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
 prettify-time sort-eggs
 )

(import scheme chicken)
(use srfi-1 data-structures extras salmonella)

(include "salmonella-common.scm")

(define (get-by-egg/action egg action log)
  (find (lambda (entry)
          (and (eq? (report-egg entry) egg)
               (eq? (report-action entry) action)))
        log))

(define (read-log-file log-file)
  (map (lambda (entry)
         (apply make-report entry))
       (with-input-from-file log-file read-file)))

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
    (or (zero? status) (= status -1))))


;; test
(define (test-status egg log) (log-get egg 'test report-status log))
(define (test-message egg log) (log-get egg 'test report-message log))
(define (test-duration egg log) (log-get egg 'test report-duration log))
(define (has-test? egg log) (not (= (test-status egg log) -1)))

;; meta-data
(define (meta-data egg log) (log-get egg 'meta-data report-message log))

(define (egg-dependencies egg log #!key with-test-dependencies? with-versions?)
  (let ((data (meta-data egg log)))
    (get-egg-dependencies data
                          with-test-dependencies?: with-test-dependencies?
                          with-versions?: with-versions?)))

(define (egg-license egg log)
  (let ((data (meta-data egg log)))
    (and-let* ((license (alist-ref 'license data)))
      (car license))))

;; doc
(define (doc-exists? egg log)
  (zero? (log-get egg 'check-doc report-status log)))


;; start & end
(define (start-time log)
  (report-duration (car log)))

(define (salmonella-info log)
  (report-message (car log)))

(define (end-time log)
  (report-duration (last log)))

(define (total-time log)
  (- (end-time log) (start-time log)))

;; statistics
(define (count-install-ok log)
  (count (lambda (egg)
           (let ((status (install-status egg log)))
             (and status (zero? status))))
         (log-eggs log)))

(define (count-install-fail log)
  (- (count-total-eggs log) (count-install-ok log)))

(define (count-test-ok log)
  (count (lambda (entry)
           (and (eq? 'test (report-action entry))
                (zero? (report-status entry))))
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

(define (count-total-eggs log #key with-skipped?)
  (+ (length (log-eggs log))
     (if with-skipped?
         (length (log-skipped-eggs log))
         0)))

(define (count-documented log)
  (count (lambda (entry)
           (and (eq? 'check-doc (report-action entry))
                (zero? (report-status entry))))
         log))

(define (count-undocumented log)
  (count (lambda (entry)
           (and (eq? 'check-doc (report-action entry))
                (not (zero? (report-status entry)))))
         log))

;; Misc
(define (prettify-time seconds)
  (cond ((zero? seconds)
         "")
        ((< seconds 60)
         (conc seconds "s"))
        ((< seconds 3600)
         (let ((mins (quotient seconds 60)))
           (conc mins "m" (prettify-time (- seconds (* 60 mins))))))
        (else
         (let ((hours (quotient seconds 3600)))
           (conc hours "h" (prettify-time (- seconds (* 3600 hours))))))))

(define (sort-eggs eggs)
  (sort eggs (lambda (e1 e2)
               (string<? (symbol->string e1)
                         (symbol->string e2)))))

) ;; end module

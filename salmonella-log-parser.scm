(use srfi-1)

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
  ;; Return a list of eggs from `log'
  (let loop ((log log)
             (eggs '()))
    (if (null? log)
        eggs
        (let ((egg (report-egg (car log))))
          (loop (cdr log)
                (if (or (not (symbol? egg))
                        (memq egg eggs))
                    eggs
                    (cons egg eggs)))))))

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
(define (check-version-duration egg log) (log-get egg 'check-version report-duration log))

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

;; doc
(define (doc-exists? egg log) (zero? (log-get egg 'doc report-status log)))


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
  (count (lambda (entry)
           (and (eq? 'install (report-action entry))
                (zero? (report-status entry))))
         log))

(define (count-install-fail log)
  (count (lambda (entry)
           (and (eq? 'install (report-action entry))
                (not (zero? (report-status entry)))))
         log))

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

(define (count-total-eggs log)
  (let ((eggs (filter symbol? (map report-egg log))))
    (length (delete-duplicates eggs eq?))))

(define (count-documented log)
  (count (lambda (entry)
           (and (eq? 'doc (report-action entry))
                (zero? (report-status entry))))
         log))

(define (count-undocumented log)
  (count (lambda (entry)
           (and (eq? 'doc (report-action entry))
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

(use salmonella-log-parser)

(define (concat l)
  (string-intersperse (map ->string l) ""))

(define (h1 . text)
  (conc "###\n### " (concat text) "\n###\n"))

(define (h2 . text)
  (conc "=== " (concat text)))


(define (view-log log-file)
  (let* ((log (read-log-file log-file))
         (eggs (sort-eggs (log-eggs log))))
    (for-each
     (lambda (egg)
       ;; Heading
       (print (h1 egg))

       ;; Installation
       (print (h2 egg " installation: ")
              (if (zero? (install-status egg log))
                  "[OK]"
                  "[FAIL]")
              "\n")
       (print (install-message egg log))

       ;; Tests
       (print (h2 egg " test: ")
              (case (test-status egg log)
                ((0 #t) "[OK]")
                ((-1) "[ -- ]")
                (else "[FAIL]"))
              "\n")
       (print (test-message egg log)))
     eggs)))

(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name))))
    (print this " <salmonella log file>"))
  (when exit-code (exit exit-code)))

(let ((args (command-line-arguments)))
  (when (null? args) (usage 1))
  (when (or (member "-h" args)
            (member "--help" args))
    (usage 0))
  (view-log (car args)))

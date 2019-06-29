(module salmonella-log-viewer ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use data-structures files)
  (use salmonella-log-parser))
 (chicken-5
  (import (chicken base)
          (chicken pathname)
          (chicken process-context)
          (chicken string))
  (import salmonella-log-parser))
 (else
  (error "Unsupported CHICKEN version.")))

(include "salmonella-common.scm")
(include "salmonella-version.scm")

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
       ;; Installation
       (let ((status (install-status egg log)))
         (when status
           ;; Only print heading when there is an install action for
           ;; the egg
           (print (h1 egg))

           (print (h2 egg " installation: ")
                  (case status
                    ((0) "[OK]")
                    (else "[FAIL]"))
                  "\n")
           (print (install-message egg log))))

       ;; Tests
       (let ((status (test-status egg log)))
         (when status
           (print (h2 egg " test: ")
                  (case status
                    ((0 #t) "[OK]")
                    ((-1) "[ -- ]")
                    (else "[FAIL]"))
                  "\n")
           (print (test-message egg log)))))
     eggs)

    ;; env info
    (print (h1 "Environment information"))
    (print (salmonella-info log))
    ))

(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name))))
    (print this " <salmonella log file>"))
  (when exit-code (exit exit-code)))

(let* ((parsed-args (parse-cmd-line (command-line-arguments)
                                    '(-h
                                      --help
                                      --version)))
       (log-files (car parsed-args))
       (args (cdr parsed-args)))

  (when (null? log-files)
    (usage 1))

  (when (or (cmd-line-arg '-h args)
            (cmd-line-arg '--help args))
    (usage 0))

  (when (cmd-line-arg '--version args)
    (print salmonella-version)
    (exit 0))

  (view-log (car log-files)))

) ;; end module

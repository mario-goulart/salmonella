(module salmonella-log-merger ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use data-structures files srfi-1 srfi-13)
  (use salmonella salmonella-log-parser))
 (chicken-5
  (import (chicken base)
          (chicken file)
          (chicken pathname)
          (chicken pretty-print)
          (chicken process-context)
          (chicken random)
          (chicken string))
  (import salmonella salmonella-log-parser)
  (include "libs/srfi-1.scm")
  (include "libs/srfi-13.scm"))
 (else
  (error "Unsupported CHICKEN version.")))

(include "salmonella-common.scm")
(include "salmonella-version.scm")

(define (merge-logs log-files)
  (let* ((logs (map read-log-file log-files))
         (first-started
          (let loop ((logs logs)
                     (first-started (caar logs)))
            (if (null? logs)
                first-started
                (let* ((log (car logs))
                       (log-start (start-time log)))
                  (loop (cdr logs)
                        (if (< log-start (report-duration first-started))
                            (car log)
                            first-started))))))
         (last-finished
          (let loop ((logs logs)
                     (last-finished (last (car logs))))
            (if (null? logs)
                last-finished
                (let* ((log (car logs))
                       (log-end (end-time log)))
                  (loop (cdr logs)
                        (if (> log-end (report-duration last-finished))
                            (last log)
                            last-finished)))))))

    (append
     (list (report->list first-started))
     (let loop ((logs logs))
       (if (null? logs)
           '()
           (append (map report->list
                        (butlast (cdr (car logs))))
                   (loop (cdr logs)))))
     (list (report->list last-finished)))))


(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name))))
    (print this " --log-file=<log file> log1 log2 ... logn")
    (when exit-code (exit exit-code))))



(let* ((parsed-args (parse-cmd-line (command-line-arguments)
                                    '(-h
                                      --help
                                      --version
                                      (--log-file))))
       (log-files (car parsed-args))
       (args (cdr parsed-args)))

  (handle-help args
               (lambda ()
                 (usage 0)))

  (when (and (null? args) (null? log-files))
    (usage 1))

  (when (cmd-line-arg '--version args)
    (print salmonella-version)
    (exit 0))

  (let ((out-file (cmd-line-arg '--log-file args)))
    (when (file-exists? out-file)
      (die out-file " already exists. Aborting."))
    (unless out-file
      (usage 1))

    (with-output-to-file out-file
      (lambda ()
        (for-each pp (merge-logs log-files))))))

) ;; end module

(use salmonella salmonella-log-parser)
(include "salmonella-common.scm")

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



(let ((args (command-line-arguments)))
  (when (null? args)
    (usage 1))
  (let ((log-files
         (remove (lambda (arg)
                   (string-prefix? "--" arg))
                 args))
        (out-file (cmd-line-arg '--log-file args)))
    (when (file-exists? out-file)
      (die out-file " already exists. Aborting."))
    (unless out-file
      (usage 1))

    (with-output-to-file out-file
      (lambda ()
        (for-each pp (merge-logs log-files))))))

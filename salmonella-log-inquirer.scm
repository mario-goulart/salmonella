(module salmonella-log-inquirer ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use data-structures files extras)
  (use salmonella salmonella-log-parser))
 ((or chicken-5 chicken-6)
  (import (chicken base)
          (chicken file)
          (chicken pathname)
          (chicken pretty-print)
          (chicken process-context)
          (chicken random)
          (chicken string))
  (import salmonella salmonella-log-parser)
  (include "libs/srfi-1.scm"))
 (else
  (error "Unsupported CHICKEN version.")))

(include "salmonella-common.scm")

(define valid-actions '(fetch install check-version test meta-data))
(define valid-parts '(message status duration))

(define (query-action log-data egg action part)
  (log-get egg
           action
           (case part
             ((message) report-message)
             ((status) report-status)
             ((duration) report-duration)
             (else (error 'query-action "Invalid part" part)))
           log-data))

(define (log-statistics log-data)
  #<#EOF
=== Summary
Total eggs: #(count-total-eggs log-data)

==== Installation
Ok: #(count-install-ok log-data)
Failed: #(count-install-fail log-data)

==== Tests
Ok: #(count-test-ok log-data)
Failed: #(count-test-fail log-data)
No tests: #(count-no-test log-data)

==== Documentation
Documented: #(count-documented log-data)
Undocumented: #(count-undocumented log-data)

==== Total run time
#(prettify-time (inexact->exact (total-time log-data)))
EOF
)

(define usage
  (let ((all-actions
         (string-intersperse (map symbol->string valid-actions) "\n"))
        (all-parts
         (string-intersperse (map symbol->string valid-parts) "\n")))
    (make-usage
     (lambda (this port)
       (display #<#EOF
#this [ -h | -help | --help ]
  Print this message.

#this --version
  Show version and exit.

#this --log-info <log file>
  Show information about the environment where salmonella was executed
  to generate the given <log-file> (e.g., environment variables, C
  compiler, CHICKEN version, salmonella command line etc.)

#this --statistics <log file>
  Show simple statistics on installation, tests and documentation from
  the log file .

#this --list-eggs <log file>
  List eggs covered in <log file>.

#this --action=<action> --egg=<egg> [ --part=<part> ] <log file>
  Query <action> and, optionally, <part> for <egg> in <log file>.

<action>s:
#all-actions

<part>s (the default part is "message"):
#all-parts

EOF
    port)))))


(let* ((parsed-args (parse-cmd-line (command-line-arguments)
                                    '(-h
                                      --help
                                      --version
                                      --list-eggs
                                      --log-info
                                      --statistics
                                      (--action)
                                      (--egg)
                                      (--part))))
       (log-files (car parsed-args))
       (args (cdr parsed-args)))

  (handle-help args
               (lambda ()
                 (usage 0)))

  (handle-version args)

  (when (null? log-files)
    (usage 1))

  (let ((log-file (car log-files)))
    (cond
     ((cmd-line-arg '--list-eggs args)
      (for-each print (log-eggs (read-log-file log-file))))
     ((cmd-line-arg '--log-info args)
      (print (salmonella-info (read-log-file log-file))))
     ((cmd-line-arg '--statistics args)
      (print (log-statistics (read-log-file log-file))))
     (else
      (let ((action-str (cmd-line-arg '--action args))
            (egg-str  (cmd-line-arg '--egg args))
            (part-str (cmd-line-arg '--part args)))
        (unless action-str
          (die "Missing --action=<action>"))
        (unless egg-str
          (die "Missing --egg=<egg>"))
        (let ((action (string->symbol action-str))
              (egg (string->symbol egg-str))
              (part (if part-str
                        (string->symbol part-str)
                        'message)))
          (unless (memq action valid-actions)
            (die "Invalid action: " action))
          (unless (memq part valid-parts)
            (die "Invalid part: " part))
          (let ((printer (if (eq? action 'meta-data)
                             pp
                             print))
                (result
                 (query-action (read-log-file log-file) egg action part)))
            (if result
                (printer result)
                (exit 2)))))))))

) ;; end module

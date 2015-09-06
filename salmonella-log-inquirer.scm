(module salmonella-log-inquirer ()

(import chicken scheme)
(use data-structures files extras)
(use salmonella salmonella-log-parser)
(include "salmonella-version.scm")
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

(define (read-log args)
  (let ((log-file (last args)))
    (read-log-file log-file)))

(define (usage #!optional exit-code)
    (let ((this (pathname-strip-directory (program-name)))
          (port (if (and exit-code (not (zero? exit-code)))
                    (current-error-port)
                    (current-output-port))))
    (display #<#EOF
#this [ -h | --help ]
#this --version
#this --log-info <log-file>
#this --statistics <log-file>
#this --list-eggs <log file>
#this --action=<action> --egg=<egg> [ --part=<part> ] <log file>

<action>s:
#(string-intersperse (map symbol->string valid-actions) "\n")

<part>s (the default part is "message"):
#(string-intersperse (map symbol->string valid-parts) "\n")
EOF
    port)
    (newline)
    (when exit-code (exit exit-code))))


(let ((args (command-line-arguments)))
  (cond
   ((null? args)
    (usage 1))
   ((or (member "-h" args)
        (member "-help" args)
        (member "--help" args))
    (usage 0))
   ((member "--version" args)
    (print salmonella-version))
   ((null? (cdr args)) ;; from now on we require the log file
    (usage 1))
   ((member "--list-eggs" args)
    (for-each print (log-eggs (read-log args))))
   ((member "--log-info" args)
    (print (salmonella-info (read-log args))))
   ((member "--statistics" args)
    (print (log-statistics (read-log args))))
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
                           print)))
          (printer (query-action (read-log args) egg action part)))))))
  )
) ;; end module

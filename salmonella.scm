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
  (import chicken)
  (include "salmonella-4.scm"))
 (chicken-5
  (import (chicken base))
  (include "salmonella-5.scm"))
 (else
  (error "Unsupported CHICKEN version.")))

(include "salmonella-common.scm")
(include "salmonella-version.scm")

) ; end module

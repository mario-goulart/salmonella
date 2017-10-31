(import (chicken data-structures)
        (chicken string))
(import test salmonella salmonella-log-parser)

;; FIXME: deduplicate
(define (string-prefix? prefix str)
  (let ((index (substring-index prefix str)))
    (and index (zero? index))))

(define log-v0 (read-log-file "salmonella.log.v0"))
(define log-v1 (read-log-file "salmonella.log.v1"))
(define log (read-log-file "salmonella.log")) ;; Log v2

(test-begin "Salmonella")

(test 0 (log-version log-v0))
(test 1 (log-version log-v1))
(test 2 (log-version log))

(test '(ansi-escape-sequences slice) (log-eggs log))
(test 0 (fetch-status 'slice log))
(test 0 (install-status 'slice log))
(test 0 (test-status 'slice log))
(test #t (has-test? 'slice log))

(test 0 (fetch-status 'ansi-escape-sequences log))
(test 0 (install-status 'ansi-escape-sequences log))
(test -1 (test-status 'ansi-escape-sequences log))
(test "" (test-message 'ansi-escape-sequences log))
(test #f (has-test? 'ansi-escape-sequences log))

(test 2 (count-install-ok log))
(test 0 (count-install-fail log))
(test 2 (count-total-eggs log))
(test 1 (count-no-test log))
(test 1 (count-test-ok log))
(test 0 (count-test-fail log))
(test 2 (count-documented log))
(test 0 (count-undocumented log))

(test 1314226508.0 (start-time log))
(test 1314226540.0 (end-time log))
(test 31.0 (total-time log))
(test #t (string-prefix? "salmonella" (salmonella-info log)))

(test #t (check-version-ok? 'slice log))
(test #t (check-version-ok? 'ansi-escape-sequences log))
(test "1.0" (egg-version 'slice log))
(test "0.1" (egg-version 'ansi-escape-sequences log))

(test "slice.egg" (car (alist-ref 'egg (meta-data 'slice log))))
(test "ansi-escape-sequences.egg"
      (car (alist-ref 'egg (meta-data 'ansi-escape-sequences log))))

(test #t (doc-exists? 'slice log))
(test #t (doc-exists? 'ansi-escape-sequences log))

(test '() (egg-dependencies 'slice log))
(test '() (egg-dependencies 'ansi-escape-sequences log))
(test-error (egg-dependencies 'srfi-13))

(test "BSD" (egg-license 'slice log))
(test "BSD" (egg-license 'ansi-escape-sequences log))

(test-end "Salmonella")

(test-exit)

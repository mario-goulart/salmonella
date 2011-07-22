(use test)

(load "../salmonella-log-parser.scm")
(load "../salmonella.scm")

(define log (read-log-file "salmonella.log"))

(test 0 (fetch-status 'slice log))
(test 0 (install-status 'slice log))
(test 0 (test-status 'slice log))
(test #t (string-prefix? "CHICKEN_INSTALL_PREFIX="
                         (test-message 'slice log)))
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

(test 1310859122.0 (start-time log))
(test 1310859131.0 (end-time log))
(test 9.0 (total-time log))
(test #t (string-prefix? "salmonella" (salmonella-info log)))

(test-exit)

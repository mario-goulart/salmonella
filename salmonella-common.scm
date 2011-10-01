(import irregex)
(use srfi-1 ports)

(define (get-egg-dependencies meta-data #!key with-test-dependencies?
                                              with-versions?)
  (define (deps key)
    (or (and-let* ((d (assq key meta-data)))
          (cdr d))
        '()))
  (map (lambda (dep)
         (if with-versions?
             dep
             (if (pair? dep)
                 (car dep)
                 dep)))
       (append (deps 'depends)
               (deps 'needs)
               (if with-test-dependencies?
                   (deps 'test-depends)
                   '()))))


(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))


(define (die . msg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (for-each display msg)
      (newline)
      (flush-output)))
  (exit 1))

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

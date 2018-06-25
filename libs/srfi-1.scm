(import (chicken fixnum))

(define (make-list len . maybe-elt)
  (##sys#check-exact len 'make-list)
  (let ((elt (cond ((null? maybe-elt) #f) ; Default value
                   ((null? (cdr maybe-elt)) (car maybe-elt))
                   (else (##sys#error 'make-list "Too many arguments to MAKE-LIST"
                                      (cons len maybe-elt))))))
    (do ((i len (fx- i 1))
         (ans '() (cons elt ans)))
        ((fx<= i 0) ans))))

(define (filter pred lst)
  (foldr (lambda (x r) (if (pred x) (cons x r) r)) '() lst))

(define (remove pred lst)
  (filter (lambda (x) (not (pred x))) lst))

(define (delete x lst #!optional (test equal?))
  (let loop ((lst lst))
    (cond ((null? lst) lst)
          ((test x (car lst))
           (loop (cdr lst)))
          (else
           (cons (car lst) (loop (cdr lst)))))))

(define (delete-duplicates lst #!optional (test equal?))
  (let loop ((lst lst))
    (if (null? lst)
        lst
        (let* ((x (car lst))
               (tail (cdr lst))
               (new-tail (loop (delete x tail test))))
          (if (equal? tail new-tail)
              lst
              (cons x new-tail))))))

(define (any pred lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
          ((pred (car lst)))
          (else (loop (cdr lst))))))

;; vvv For salmonella-log-parser vvv
(define (last lst)
  (let loop ((lst lst))
    (if (null? (cdr lst))
        (car lst)
        (loop (cdr lst)))))

(define (find pred lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
          ((pred (car lst)) (car lst))
          (else (loop (cdr lst))))))

(define (count pred list1 . lists)
  (if (pair? lists)
      ;; N-ary case
      (let lp ((list1 list1) (lists lists) (i 0))
        (if (null? list1) i
            (receive (as ds) (##srfi1#cars+cdrs lists)
              (if (null? as) i
                  (lp (cdr list1) ds
                      (if (apply pred (car list1) as) (fx+ i 1) i))))))
      ;; Fast path
      (let lp ((lis list1) (i 0))
        (if (null? lis) i
            (lp (cdr lis) (if (pred (car lis)) (fx+ i 1) i))))))

;; vvv For salmonella-cmd vvv
(define (list-tabulate n proc)
  (let loop ((i 0))
    (if (fx>= i n)
        '()
        (cons (proc i) (loop (fx+ i 1))))))

(define (iota n #!optional (start 0))
  (list-tabulate n (lambda (i) (+ i start))))

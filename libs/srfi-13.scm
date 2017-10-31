(import (chicken data-structures))

(define (string-prefix? prefix str)
  (let ((index (substring-index prefix str)))
    (and index (zero? index))))

(define (string-pad-right s n char)
  (let ((len (string-length s)))
    (if (<= n len)
        (substring s (- len n) len)
        (let ((pad (make-string (- n len) char)))
          (string-append s pad)))))


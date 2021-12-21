#lang racket
(require rackunit rackunit/text-ui)

;### Зад 3
; Намира най-малкото число, което се среща поне 2 пъти в списъка `l`.
(define (min-duplicate l)
  (define (helper l result found)
   (if (null? l) result (if (duplicate-in (car l) (cdr l)) (if found (if (< (car l) result) (helper (cdr l) (car l) #t) (helper (cdr l) result #t)) (helper (cdr l) (car l) #t)) (helper (cdr l) result found)))
    )
  (helper l 0 #f)
   )

(define (duplicate-in x list)
 (if (null? list) #f (if (eqv? x (car list)) #t (duplicate-in x (cdr list))))
  )


(run-tests
  (test-suite "min-duplicate tests"
    (check-eq? (min-duplicate '(-8 -8))
               -8)
    (check-eq? (min-duplicate '(1 2 3 4 4 5 6))
               4)
    (check-eq? (min-duplicate '(5 1 2 3 4 5 3 6 2 3 2 3 2 3))
               2)
    (check-eq? (min-duplicate '(3 2 2 2 1 1))
               1))
  'verbose)

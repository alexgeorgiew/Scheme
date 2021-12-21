#lang racket
(require rackunit rackunit/text-ui)

;### Зад 2
; Броят на наредените двойки цели числа от интервала [`a`,`b`],
; които имат най-голям общ делител равен на `n`.
(define (count-pairs-gcd n a b)
  
  (define (helper n start end i j res)
    (cond ((> i end) res)
          ((> j end) (helper n start end (+ i 1) start res))
          ((= n (gcd i j)) (helper n start end i (+ j 1) (+ res 1)))
          (else (helper n start end i (+ j 1) res))
          )
    )
  
  (helper n a b a a 0)
  
)

(define (abs x)
  (if (< x 0) (* -1 x) x)
  )

(define (gcd a b)
  (gcd-abs (abs a) (abs b))
  )

(define (gcd-abs a b)
  (if (= a b) a (if (> a b) (gcd-abs (- a b) b) (gcd-abs a (- b a))))
  )

(run-tests
  (test-suite "count-pairs-gcd tests"
    (check-eq? (count-pairs-gcd 10 1 11)
               1)
    (check-eq? (count-pairs-gcd 3 1 11)
               7)
    (check-eq? (count-pairs-gcd 16 1 11)
               0)
    (check-eq? (count-pairs-gcd 4 1 11)
               3))
  'verbose)

#lang racket
(require rackunit rackunit/text-ui)

;### Зад 1
; Дали `x` е автоморфно?
; Едно число е автоморфно, ако квадратът му завършва на него.
(define (automorphic? x)
  (define (helper x x^2)
   (if (= x 0) #t (if (= (remainder x^2 10) (remainder x 10)) (helper (quotient x 10) (quotient x^2 10)) #f))
    )
  (helper x (* x x))
  )

(run-tests
  (test-suite
    "automorphic? tests"
    (check-false (automorphic? 2))
    (check-false (automorphic? 3))
    (check-false (automorphic? 4))
    (check-false (automorphic? 7))
    (check-false (automorphic? 8))
    (check-false (automorphic? 9))
    (check-false (automorphic? 10))

    (check-true (automorphic? 1))
    (check-true (automorphic? 5))
    (check-true (automorphic? 6))
    (check-true (automorphic? 25))
    (check-true (automorphic? 76))
    (check-true (automorphic? 376))
    (check-true (automorphic? 625))
    (check-true (automorphic? 9376)))
  'verbose)

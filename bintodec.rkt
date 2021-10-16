#lang racket
(define (bintodec x)
  (define (helper curr step answer)
    (if (= curr 0)
        answer
        (helper (quotient curr 10) (* step 2) (+ answer (* (remainder curr 10) step))))
    )
  (helper x 1 0)
  )
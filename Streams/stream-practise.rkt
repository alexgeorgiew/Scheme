#lang racket

(define the-empty-stream '())
(define (cons-stream h t) (cons h (delay t)))
(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)


(define (stream-range-generator a b)
  (if (> a b) '() (cons a (delay (stream-range-generator (+ a 1) b))))
  )

(define (stream-range a b) (stream-range-generator a b))
(define range (stream-range 1 10))

(define (stream-ref s n)
  (if (= n 0) (head s) (stream-ref (tail s) (- n 1)))  
  )

(define (add2 x)
  (+ x 2))

(define (stream-map s f)
 (if (empty-stream? s) '() (cons (f (car s)) (delay (stream-map (tail s) f))))
  )

(define (stream-filter s p)
  (if (p (head s)) (cons (head s) (delay (stream-filter (tail s) p))) (stream-filter (tail s) p))
        )

(define (stream-fold s acc f)
 (if (empty-stream? s) acc (stream-fold (tail s) (f acc (head s)) f))
  )

(define (stream->list s)   ;s has end
  (if (empty-stream? s) '() (cons (head s) (stream->list (tail s))))
  )

(define (stream-drop s n)
  (if (empty-stream? s) the-empty-stream (if (= n 0) s (stream-drop (tail s) (- n 1)))) 
  )

(define (stream-take s n)
  (if (= n 0) the-empty-stream (cons (head s) (delay (stream-take (tail s) (- n 1)))))
  )

(define (natural-generator n)
  (cons n (delay (natural-generator (+ n 1)))))

(define naturals (natural-generator 0))

(define (fibonacci-generator a b)
  (cons (+ a b) (delay (fibonacci-generator b (+ a b))))
  )

(define fibonacci (cons 0 (delay (fibonacci-generator 0 1))))

(define (stream-append s1 s2) (cons s1 (delay s2)))

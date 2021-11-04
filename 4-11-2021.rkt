#lang racket


(define (last l)
   (if (null? (cdr l) ) (car l)
       (last (cdr l)))
  )

(define (insert x n l)
  (if (= n 1) (cons x  l)
      (cons (car l) (insert x (- n 1) (cdr l))))
  )

(define (remove n l)
    (if (= n 1) (cdr l)
      (cons (car l) (remove (- n 1) (cdr l))))
  )

(define (remove-all x l)
  (if (null? l) '()
      (if (= (car l) x) (remove-all x (cdr l)) (cons (car l) (remove-all x (cdr l))))
      )
  )


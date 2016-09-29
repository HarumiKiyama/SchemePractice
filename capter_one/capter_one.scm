#! /usr/bin/env guile
(define (sum l)
  (if (null? (cdr l)) (car l)
      (+ (car l) (sum (cdr l)))))
(define (multi-sum x . y)
  (if (null? y) x
      (+ x (apply multi-sum y))))


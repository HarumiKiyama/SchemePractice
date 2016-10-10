#! /bin/env guile
!#
;; define atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((eq? a (car l)) #t)
     (else (member? a (cdr l))))))

(define rember?
  (lambda (a lat)
    (cond
     ((null? lat) lat)
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat)
                 (rember? a (cdr lat))) ))))
(rember? 7 '(1 3 5 2 7 39))

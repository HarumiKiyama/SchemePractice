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

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) lat)
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat))) ))))
(define (first l)
  (cond
   ((null? l) l)
   (else (cons (car (car l))
               (first (cdr l))))))

(define (insertR new old lat)
  (cond
   ((null? lat) lat)
   ((eq? old (car lat)) (cons old (cons new (cdr lat))))
   (else (cons (car lat)
               (insertR old new (cdr lat))))))
(define (insertL new old l)
  (cond
   ((null? l) l)
   ((eq? old (car l)) (cons new l))
   (else (cons (car l)
               (insertL old new (cdr l))))))
(define (subset new old l)
  (cond
   ((null? l) l)
   ((eq? old (car l)) (cons new (cdr l)))
   (else (cons (car l)
               (subset old new (cdr l))))))
(define (subset2 new old1 old2 l)
  (cond
   ((null? l) l)
   ((or (eq? old1 (car l))
        (eq? old2 (car l))) (cons new (cdr l)))
   (else (cons (car l)
               (subset2 new old1 old2 (cdr l))))))

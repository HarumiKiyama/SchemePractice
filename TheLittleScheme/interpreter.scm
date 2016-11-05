(define (atom? n)
  (and (not (pair? n))
       (not (null? n))))

(define (else? n)
  (cond
   ((atom? n)
    (eq? n 'else))
   (else #f)))

(define (first l)
  (car l))

(define (second l)
  (car (cdr l)))

(define (third l)
  (car (cdr (cdr l))))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (*const e table)
  (cond
   ((number? e) e)
   ((eq? e #f) #f)
   ((eq? e #t) #t)
   (else
    (build 'primitive e))))

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

(define (lookup-in-entry-help name names values entry-f)
  (cond
   ((null? names) (entry-f name))
   ((eq? (car names) name)
    (car values))
   (else
    (lookup-in-entry-help name (cdr names)
                          (cdr values)
                          entry-f))))

(define (lookup-in-table name table table-f)
  (cond
   ((null? table) (table-f name))
   (else
    (lookup-in-entry name (car table) (lambda (name)
                                        (lookup-in-table name (cdr table) table-f))))))


(define (*quote e table)
  (text-of e))

(define (initial-table name)
  (car '()))

(define (expression-to-action e)
  (cond
   ((atom? e)
    (atom-to-action e))
   (else (list-to-action e))))
                                        ;TODO: COMPLETE THIS FUNCTION
(define (atom-to-action e)
  )

(define (list-to-action l)
  (cond
   ((atom? (car l))
    (cond
     ((eq? (car l) 'quote) *quote)
     ((eq? (car l) 'lambda) *lambda)
     ((eq? (car l) 'cond) *cond)
     (else *application)))
   (else *application)))

(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*identifier e table)
  (lookup-in-table e table initial-table))

(define (*lambda e table)
  (build 'non-primitive
         (cons table (cdr e))))

(define text-of second)

(define table-of first)

(define formals-of second)

(define body-of third)

(define question-of first)

(define answer-of second)

(define (evcon lines table)
  (cond
   ((else? (question-of (car lines)))
    (meaning (answer-of (car lines)) table))
   ((meaning (question-of (car lines)))
    (meaning (answer-of (car lines)) table))
   (else (evcon (cdr lines) table))))

(define (*cond e table)
  (evcon (cdr e) table))

(define (evlis args table)
  (cond
   ((null? args) '())
   (else
    (cons (meaning (car args) table)
          (evlis (cdr args) table)))))

(define (*application e table)
  (apply
   (meaning (functions-of e) table)
   (evlis (arguments-of e) table)))

(define functions-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

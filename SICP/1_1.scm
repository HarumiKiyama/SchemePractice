;; SCIP 1_1 Assignments
;; 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3)))))
  )

(define (f' n)
  (define (f_iter a b c count)
    (if (= count 0)
        a
        (f_iter b c (+ a (* 2 b) (* 3 c))
                (- count 1)
                ))
    )
  (f_iter 0 1 2 n)
  )

(f 12)
(f' 12)

;; 1.12

(define (pascal r c)
  (if (or (= c 1) (= r c))
      1
      (+ (pascal (- r 1)
                 (- c 1))
         (pascal (- r 1)
                 c)
         )))

(pascal 3 2)

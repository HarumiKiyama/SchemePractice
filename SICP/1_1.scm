;; SCIP 1_1 Assignments
;; 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3)))))
  )

(define (f_iter a b c count)
  (if (< count 3)
      a
      (f_iter (+ a (* 2 b) (* 3 c))
              a b (- count 1)
              ))
  )

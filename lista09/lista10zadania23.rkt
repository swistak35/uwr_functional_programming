#lang racket
(define (acount lst)
  (if (empty? lst)
      0 
      (let (
            [h (car lst)]
            [t (cdr lst)]
           )
        (+ (if (pair? h) 1 0) (acount t))
      )
  )
)

(define (count-change amount lst)
  (if (equal? amount 0)
      1
      (if (or (empty? lst) (amount . < . 0))
          0
          (+ (count-change (- amount (car lst)) lst) (count-change amount (cdr lst)))
      )
  )
)

(define (runtest) (= (count-change 100 '(1 5 10 25 50)) 292))

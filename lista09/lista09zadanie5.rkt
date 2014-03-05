#lang racket

; Zapytac sie o konwencje ()

; Delete first occurrence of x in lst
(define (delete lst x)
  (if (eq? (car lst) x)
      (cdr lst)
      (cons (car lst) (delete (cdr lst) x))))

; Check if one
(define (one? x) (equal? x 1))

; Main derivation procedure
(define (deriv lst x)
  
  ; Helper function for multiplication derivation
  (define (derive-mult lst e)
    (let ([lste (delete lst e)])
      (clean (cons '* (cons (derive e) lste)))))
  
  ; Expression cleaner
  (define (clean lst)
    (let ([lst2 (include-singletons lst)])
      (cond 
        [(equal? (car lst2) '+) (cons '+ (sum-constants (cdr lst2)))]
        [(equal? (car lst2) '*) (cons '* (product-constants (cdr lst2)))]
        [else lst2]
      )))
  ; Return sum of numeric values in list, and returns that list with one sum.
  ;   Warning: it may return empty list, if there was only constants, and their sum was 0.
  (define (sum-constants lst [acc-list empty] [acc-sum 0])
    (cond
      [(empty? lst) (if (zero? acc-sum) acc-list (cons acc-sum acc-list))]
      [(number? (car lst)) (sum-constants (cdr lst) acc-list (+ acc-sum (car lst)))]
      [else (sum-constants (cdr lst) (cons (car lst) acc-list) acc-sum)]))
  
  ; Similar function to above, but it's calculating product
  ;   Note: it returns '(0) if there was 0 in the list
  ;   Warning: similar to warning in sum-constants, but with 1
  (define (product-constants lst [acc-list empty] [acc-product 1])
    (cond
      [(empty? lst) (if (one? acc-product) acc-list (cons acc-product acc-list))]
      [(and (number? (car lst)) (zero? (car lst))) '(0)]
      [(number? (car lst)) (product-constants (cdr lst) acc-list (* acc-product (car lst)))]
      [else (product-constants (cdr lst) (cons (car lst) acc-list) acc-product)]))
  
  ; Check if it's constant, returns it, if true, otherwise, returns '()
  (define (singletonize x)
    (cond
      [(empty? (cdr x)) (car x)]
      [(and (equal? (car x) '+) (empty? (cdr x))) 0] ; case for warning in sum-constants
      [(and (equal? (car x) '+) (empty? (cddr x))) (cadr x)]
      [(and (equal? (car x) '*) (empty? (cdr x))) 1] ; case for warning in product-constants
      [(and (equal? (car x) '*) (empty? (cddr x))) (cadr x)]
      [else empty]
      ))
    
  ; If list has singletons, include them to the list.
  ;   Note: it can detect (+ _) or (* _) singletons.
  (define (include-singletons lst)
    (if (empty? lst)
        empty
        (if (cons? (car lst))
            (let ([singleton (singletonize (car lst))])
              (if (empty? singleton)
                  (cons (car lst) (include-singletons (cdr lst)))
                  (cons singleton (include-singletons (cdr lst)))))
            (cons (car lst) (include-singletons (cdr lst))))))
  
  ; Main deriving function
  (define (derive expr)
    (cond
      [(and (symbol? expr) (equal? expr x)) '(1)]
      [(and (list? expr) (equal? (car expr) '+)) (clean (cons '+ (map (λ (e) (clean (derive e))) (cdr expr))))]
      [(and (list? expr) (equal? (car expr) '*)) (clean (cons '+ (map (λ (e) (derive-mult (cdr expr) e)) (cdr expr))))]
      [(or (symbol? expr) (number? expr)) '(0)]
      [else (begin (write "Nieznane wyrażenie") (write expr))]
      ))
  
  ; Run deriving function
  (car (clean (cons (derive lst) empty))))

(define (run-tests)
  (define test-cases (list
                      (cons '(* (* x y) (+ x 3)) '(+ (* x y) (* y (+ x 3))))
                      (cons '(* x y) 'y)
                      (cons '(+ (* x y) (* y (+ x 3))) '(+ y y))
                     ))
  (define (run-test test)
    (begin
      (display "Wyrażenie: ")
      (write (car test))
      (display "\nPrawdziwy wynik: ")
      (write (cdr test))
      (display "\nObliczony wynik: ")
      (write (deriv (car test) 'x))
      (display "\n----------\n")))
  (map run-test test-cases))
#lang racket
(require racket/draw)

; Constants
(define PI 3.14159)
(define ANGLE (PI . / . 4))
(define LENGTH 20)

; Typedefs
(define (mk-mobile left right) (cons left right))
(define (mk-branch length struct) (cons length struct))

; Getters
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-struct branch) (cdr branch))

; Utilities
(define (total-weight mobile)
  (define (total-branch-weight branch)
    (let 
      ([struct (branch-struct branch)])
      (if (number? struct) struct (total-weight struct))
    )
  )
  (if (number? mobile)
      mobile
      (+ (total-branch-weight (left-branch mobile)) (total-branch-weight (right-branch mobile)))
  )
)

(define (balanced? mobile)
  (if (number? mobile) #t 
  (letrec
    (
     [left (left-branch mobile)]
     [right (right-branch mobile)]
     [left-struct (branch-struct left)]
     [right-struct (branch-struct right)]
    )
    (and
      (balanced? left-struct)
      (balanced? right-struct)
      (=
       (* (branch-length left) (total-weight left-struct))
       (* (branch-length right) (total-weight right-struct))
      )
    )
  )
  )
)

(define (left-span mobile)
  (if (number? mobile)
      0
      (let ([left (left-branch mobile)]) (+ (branch-length left) (left-span (branch-struct left))))))

(define (right-span mobile)
  (if (number? mobile)
      0
      (let ([right (right-branch mobile)]) (+ (branch-length right) (right-span (branch-struct right))))))

(define (depth mobile)
  (if (number? mobile)
      1
      (+ 1 (max (depth (branch-struct (left-branch mobile))) (depth (branch-struct (right-branch mobile)))))))

(define (draw-mobile mobile)
  (define target (make-bitmap 800 600))
  (define dc (new bitmap-dc% [bitmap target]))
  (define DIFFX (* (sin ANGLE) LENGTH))
  (define DIFFY (* (cos ANGLE) LENGTH))
  (define (draw-weight weight x y)
    (define span (/ LENGTH 3))
    (send dc draw-rectangle (- x span) y (* span 2) (* weight span))
    )
  (define (draw-struct struct x y)
    (if (number? struct)
        (draw-weight struct x y)
        (let (
              [left-diff-x (- x (* DIFFX (branch-length (left-branch struct))))]
              [left-diff-y (+ y (* DIFFY (branch-length (left-branch struct))))]
              [right-diff-x (+ x (* DIFFX (branch-length (right-branch struct))))]
              [right-diff-y (+ y (* DIFFY (branch-length (right-branch struct))))]
              )
          (send dc draw-line x y left-diff-x left-diff-y)
          (send dc draw-line x y right-diff-x right-diff-y)
          (draw-struct (branch-struct (left-branch struct)) left-diff-x left-diff-y)
          (draw-struct (branch-struct (right-branch struct)) right-diff-x right-diff-y)
          )))
  (draw-struct mobile 400 20)
  (send target save-file "konstrukcja.png" 'png)
  )

; Tests
(define test-mobile-1 (mk-mobile (mk-branch 6 (mk-mobile (mk-branch 4 1) (mk-branch 2 2))) (mk-branch 9 (mk-mobile (mk-branch 1 1) (mk-branch 1 1)))))
(define test-mobile-2 (mk-mobile (mk-branch 3 5) (mk-branch 2 (mk-mobile (mk-branch 1 6) (mk-mobile 4 7)))))

(define (test-1) (eq? #t (balanced? test-mobile-1)))
(define (test-2) (eq? #f (balanced? test-mobile-2)))
(define (test-3) (= 5 (total-weight test-mobile-1)))
(define (test-4) (= 18 (total-weight test-mobile-2)))
(define (all-tests) (and (test-1) (test-2) (test-3) (test-4)))

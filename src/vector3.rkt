#lang racket

(require "math_tools.rkt")

(provide (all-defined-out))

(struct vector3 (x y z))

(define (vector3->list v)
  (list (vector3-x v) (vector3-y v) (vector3-z v)))
(define (list->vector3 l)
  (vector3 (car l) (cadr l) (caddr l)))

(define (vector3->x v)
  (vector3-x v))
(define (vector3->y v)
  (vector3-y v))
(define (vector3->z v)
  (vector3-z v))
(define (vector3->value v i)
  (case i
    [(0) (vector3-x v)]
    [(1) (vector3-y v)]
    [(2) (vector3-z v)]))

(define (minus-v3 v)
  (unop-v3 - v))
(define (add-v3 v1 v2)
  (binop-v3 + v1 v2))
(define (sub-v3 v1 v2)
  (binop-v3 - v1 v2))
(define (mul-v3 v1 v2)
  (binop-v3 * v1 v2))
(define (div-v3 v1 v2)
  (binop-v3 / v1 v2))

(define (dot-v3 v1 v2)
  (+ (* (vector3-x v1) (vector3-x v2)) (* (vector3-y v1) (vector3-y v2)) (* (vector3-z v1) (vector3-z v2))))
(define (cross-v3 v1 v2)
  (vector3 (- (* (vector3-y v1) (vector3-z v2)) (* (vector3-z v1) (vector3-y v2)))
           (- (* (vector3-z v1) (vector3-x v2)) (* (vector3-x v1) (vector3-z v2)))
           (- (* (vector3-x v1) (vector3-y v2)) (* (vector3-y v1) (vector3-x v2)))))

(define (eq-v3? v1 v2)
  (foldr (lambda (x y) (and x y)) #t (vector3->list (binop-v3 equal? v1 v2))))
(define (ne-v3? v1 v2)
  (not (eq-v3? v1 v2)))
(define (le-v3? v1 v2)
  (foldr (lambda (x y) (and x y)) #t (vector3->list (binop-v3 <= v1 v2))))
(define (lt-v3? v1 v2)
  (foldr (lambda (x y) (and x y)) #t (vector3->list (binop-v3 < v1 v2))))
(define (ge-v3? v1 v2)
  (foldr (lambda (x y) (and x y)) #t (vector3->list (binop-v3 >= v1 v2))))
(define (gt-v3? v1 v2)
  (foldr (lambda (x y) (and x y)) #t (vector3->list (binop-v3 > v1 v2))))

(define (mind-v3 v)
  (cond [(and (< (vector3-x v) (vector3-y v)) (< (vector3-x v) (vector3-z v)))
         0]
        [(< (vector3-y v) (vector3-z v))
         1]
        [else
         2]))
(define (maxd-v3 v)
  (cond [(and (> (vector3-x v) (vector3-y v)) (> (vector3-x v) (vector3-z v)))
         0]
        [(> (vector3-y v) (vector3-z v))
         1]
        [else
         2]))
(define (minv-v3 v)
  (min (vector3-x v) (vector3-y v) (vector3-z v)))
(define (maxv-v3 v)
  (max (vector3-x v) (vector3-y v) (vector3-z v)))

(define (sqrt-v3 v)
  (unop-v3 sqrt v))
(define (pow-v3 v e)
  (unop-v3 (lambda (x) (exp x e) v)))
(define (abs-v3 v)
  (unop-v3 abs v))
(define (min-v3 v1 v2)
  (binop-v3 min v1 v2))
(define (max-v3 v1 v2)
  (binop-v3 max v1 v2))
(define (round-v3 v)
  (unop-v3 round v))
(define (floor-v3 v)
  (unop-v3 floor v))
(define (ceil-v3 v)
  (unop-v3 ceiling v))
(define (trunc-v3 v)
  (unop-v3 truncate v))
(define (clamp-v3 v low high)
  (unop-v3 (lambda (x) (clamp x low high)) v))
(define (lerp-v3 a v1 v2)
  (add-v3 v1 (mul-v3 (sub-v3 v2 v1) a)))
(define (permute-v3 v x y z)
  (vector3 (vector3->value v x) (vector3->value v y) (vector3->value v z)))

(define (unop-v3 f v)
  (vector3 (f (vector3-x v))
           (f (vector3-y v))
           (f (vector3-z v))))
(define (binop-v3 f v1 v2)
  (cond [(not (vector3? v2))
         (vector3 (f (vector3-x v1) v2)
                  (f (vector3-y v1) v2)
                  (f (vector3-z v1) v2))]
        [(not (vector3? v1))
         (vector3 (f v1 (vector3-x v2))
                  (f v1 (vector3-y v2))
                  (f v1 (vector3-z v2)))]
        [else
         (vector3 (f (vector3-x v1) (vector3-x v2))
                  (f (vector3-y v1) (vector3-y v2))
                  (f (vector3-z v1) (vector3-z v2)))]))
        
(define (norm2s-v3 v)
  (dot-v3 v v))
(define (norm2-v3 v)
  (sqrt (norm2s-v3 v)))
(define (normalize-v3 v)
  (div-v3 v (norm2-v3 v)))
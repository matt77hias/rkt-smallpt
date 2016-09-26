#lang racket

(require "vector3.rkt")
(require "ray.rkt")

(provide (all-defined-out))

(struct sphere (r p e f reflection-t))

(define epsilon-sphere 1.0e-4)

(define (sphere->r s)
  (sphere-r s))
(define (sphere->p s)
  (sphere-p s))
(define (sphere->e s)
  (sphere-e s))
(define (sphere->f s)
  (sphere-f s))
(define (sphere->reflection-t s)
  (sphere-reflection-t s))

(define (sphere->intersect s r)
  (let* ([op (sub-v3 (sphere->p s) (ray->o r))]
         [dop (dot-v3 (ray->d r) op)]
         [discriminant (+ (- (* dop dop) (dot-v3 op op)) (* (sphere->r s) (sphere->r s)))])
         (if (< discriminant 0)
             (list #f)
             (let* ([sdiscriminant (sqrt discriminant)]
                    [smin (- dop sdiscriminant)]
                    [smax (+ dop sdiscriminant)])
                   (if (and (< (ray->tmin r) smin) (< smin (ray->tmax r)))
                       (list #t smin)
                       (if (and (< (ray->tmin r) smax) (< smax (ray->tmax r)))
                           (list #t smax)
                           (list #f)))))))
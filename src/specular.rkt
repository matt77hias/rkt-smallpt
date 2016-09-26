#lang racket

(require "vector3.rkt")
(require "rng.rkt")

(provide (all-defined-out))

(define (reflectance0 n1 n2)
  (let ([sr (/ (- n1 n2) (+ n1 n2))])
    (* sr sr)))

(define (schlick-reflectance n1 n2 c)
  (let ([r0 (reflectance0 n1 n2)])
    (+ r0 (* (- 1.0 r0) (* c c c c c)))))

(define (ideal-specular-reflect d n)
  (sub-v3 d (mul-v3 (* 2.0 (dot-v3 n d)) n)))

(define (ideal-specular-transmit d n nout nin)
  (let* ([dRe (ideal-specular-reflect d n)]
         [out->in (< (dot-v3 n d) 0)]
         [nl (if out->in n (minus-v3 n))]
         [nn (if out->in (/ nout nin) (/ nin nout))]
         [cos-theta (dot-v3 d nl)]
         [cos2-phi (- 1.0 (* nn nn (- 1.0 (* cos-theta cos-theta))))])
    (if (< cos2-phi 0)
        (list dRe 1.0)
        (let* ([dTr (normalize-v3 (sub-v3 (mul-v3 nn d) (mul-v3 nl (+ (* nn cos-theta) (sqrt cos2-phi)))))]
               [c (if out->in (+ 1.0 cos-theta) (- 1.0 (dot-v3 dTr n)))]
               [re (schlick-reflectance nout nin c)]
               [prRe (+ 0.25 (* 0.5 re))]
               [u (uniform-float)])
          (if (< u prRe)
              (list dRe (/ re prRe))
              (list dTr (/ (- 1.0 re) (- 1.0 prRe))))))))
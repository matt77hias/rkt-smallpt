#lang racket

(require "vector3.rkt")

(provide (all-defined-out))

(define (uniform-sample-on-hemisphere u1 u2)
  (let* ([sin-theta (sqrt (max 0.0 (- 1.0 (* u1 u1))))]              
	 [phi (* 2.0 pi u2)])
        (vector3 (* (cos phi) sin-theta) (* (sin phi) sin-theta) u1)))

(define (cosine-weighted-sample-on-hemisphere u1 u2)
  (let* ([cos-theta (sqrt (- 1.0 u1))]
	 [sin-theta (sqrt u1)]
         [phi (* 2.0 pi u2)])
        (vector3 (* (cos phi) sin-theta) (* (sin phi) sin-theta) cos-theta)))
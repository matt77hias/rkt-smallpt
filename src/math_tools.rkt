#lang racket

(provide (all-defined-out))

(define pi 3.14159265358979323846)
(define positive-infinity 1.0e20)

(define (clamp x low high)
  (cond [(< x low) low]
        [(> x high) high]
        [else x]))

(define (float->byte x gamma)
  (exact-truncate (clamp (* 255.0 (expt x (/ 1.0 gamma))) 0.0 255.0)))
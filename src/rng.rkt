#lang racket

(provide (all-defined-out))

(define (seed-rng [seed 606418532])
  (random-seed seed))

(define (uniform-float)
  (random))

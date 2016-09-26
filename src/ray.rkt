#lang racket

(require "vector3.rkt")

(provide (all-defined-out))

(struct ray (o d tmin tmax depth))

(define (ray->o r)
  (ray-o r))
(define (ray->d r)
  (ray-d r))
(define (ray->tmin r)
  (ray-tmin r))
(define (ray->tmax r)
  (ray-tmax r))
(define (ray->depth r)
  (ray-depth r))

(define (ray->eval r t)
  (add-v3 (ray->o r) (mul-v3 (ray->d r) t)))
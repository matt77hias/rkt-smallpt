#lang racket

(require "math_tools.rkt")
(require "vector3.rkt")

(provide (all-defined-out))

(define gamma 2.2)

(define (write-ppm width height Ls fname)
  (call-with-output-file fname #:exists 'truncate
    (lambda (stream)
      (begin
        (display (format "P3~n~v ~v~n255~n" width height) stream)
        (write-ppm-Ls stream Ls (* width height))))))

(define (write-ppm-Ls stream Ls length)
  (case length
    [(0) #t]
    [(1) (write-ppm-L stream (car Ls))]
    [else (begin
            (write-ppm-L stream (car Ls))
            (display " " stream)
            (write-ppm-Ls stream (cdr Ls) (- length 1)))]))
      
(define (write-ppm-L stream L)
	(display (format "~v ~v ~v"
                         (float->byte (vector3->x L) gamma)
                         (float->byte (vector3->y L) gamma)
                         (float->byte (vector3->z L) gamma) )
                 stream))
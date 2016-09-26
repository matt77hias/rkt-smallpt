#lang racket

(require "math_tools.rkt")
(require "vector3.rkt")
(require "ray.rkt")
(require "rng.rkt")
(require "sampling.rkt")
(require "image_io.rkt")

(require "specular.rkt")
(require "sphere.rkt")

(provide main)

; Scene
(define refractive-index-out 1.0)
(define refractive-index-in 1.5)
	
(define default-scene (list
               (sphere 1.0e5 (vector3 100001 40.8 81.6)   (vector3 0.0 0.0 0.0)    (vector3 0.75 0.25 0.25)    "diffuse")
               (sphere 1.0e5 (vector3 -99901 40.8 81.6)   (vector3 0.0 0.0 0.0)    (vector3 0.25 0.25 0.75)    "diffuse")
               (sphere 1.0e5 (vector3 50.0 40.8 1.0e5)    (vector3 0.0 0.0 0.0)    (vector3 0.75 0.75 0.75)    "diffuse")
               (sphere 1.0e5 (vector3 50.0 40.8 -99830)   (vector3 0.0 0.0 0.0)    (vector3 0.0 0.0 0.0)       "diffuse")
               (sphere 1.0e5 (vector3 50.0 1.0e5 81.6)    (vector3 0.0 0.0 0.0)    (vector3 0.75 0.75 0.75)    "diffuse")
               (sphere 1.0e5 (vector3 50.0 -99918.4 81.6) (vector3 0.0 0.0 0.0)    (vector3 0.75 0.75 0.75)    "diffuse")
               (sphere 16.5  (vector3 27.0 16.5 47.0)     (vector3 0.0 0.0 0.0)    (vector3 0.999 0.999 0.999) "specular")
               (sphere 16.5  (vector3 73.0 16.5 78.0)     (vector3 0.0 0.0 0.0)    (vector3 0.999 0.999 0.999) "refractive")
               (sphere 600.0 (vector3 50.0 681.33 81.6)   (vector3 12.0 12.0 12.0) (vector3 0.0 0.0 0.0)       "diffuse")))

; Scene intersect
(define (scene->intersect scene r)
  (scene->intersect-acc scene r 0 0))
(define (scene->intersect-acc scene r ctmax csphere)
  (if (pair? scene)
      (let ([hit-record (sphere->intersect (car scene) r)])
        (if (car hit-record)
            (scene->intersect-acc (cdr scene) (ray (ray->o r) (ray->d r) (ray->tmin r) (cadr hit-record) (ray->depth r)) (cadr hit-record) (car scene))
            (scene->intersect-acc (cdr scene) r ctmax csphere)))
      (if (equal? csphere 0)
          (list #f)
          (list #t ctmax csphere))))

; Radiance
(define (radiance scene r)
  (let ([hit-record (scene->intersect scene r)])
    (if (not (car hit-record))
        (vector3 0.0 0.0 0.0)
        (let* ([p (ray->eval r (cadr hit-record))]
               [s (caddr hit-record)]
               [n (normalize-v3 (sub-v3 p (sphere->p s)))]
               [rr-record  (if (> (ray->depth r) 4)
                               (let ([prC (maxv-v3 (sphere->f s))])
                                 (if (>= (uniform-float) prC)
                                     (list #t)
                                     (list #f (div-v3 (sphere->f s) prC))))
                               (list #f (sphere->f s)))])
          (if (car rr-record)
              (sphere->e s)
              (let* ([record (case (sphere->reflection-t s)
                              [("refractive") (ideal-specular-transmit (ray->d r) n refractive-index-out refractive-index-in)]
                              [("specular") (list (ideal-specular-reflect (ray->d r) n) 1.0)]
                              [else (let* ([w (if (< (dot-v3 n (ray->d r)) 0) n (minus-v3 n))]
                                           [u1 (if (> (abs (vector3->x w)) 0.1) (vector3 0.0 1.0 0.0) (vector3 1.0 0.0 0.0))]
                                           [u (normalize-v3 (cross-v3 u1 w))]
                                           [v (cross-v3 w u)]
                                           [dd (cosine-weighted-sample-on-hemisphere (uniform-float) (uniform-float))]
                                           [ndd (normalize-v3 (add-v3 (mul-v3 (vector3->z dd) w) (add-v3 (mul-v3 (vector3->x dd) u) (mul-v3 (vector3->y dd) v))))])
                                           (list ndd 1.0))])]
                     [d (car record)]
                     [f (mul-v3 (cadr record) (sphere->f s))]
                     [L (radiance scene (ray p d epsilon-sphere positive-infinity (+ 1 (ray->depth r))))])
                (add-v3 (sphere->e s) (mul-v3 f L))))))))

; Camera
(struct camera (eye cx cy gaze))

(define (camera->eye cam)
  (camera-eye cam))
(define (camera->cx cam)
  (camera-cx cam))
(define (camera->cy cam)
  (camera-cy cam))
(define (camera->gaze cam)
  (camera-gaze cam))

(define (default-camera width height)
  (let* ([eye (vector3 50.0 52.0 295.6)]
         [gaze (normalize-v3 (vector3 0.0 -0.042612 -1.0))]
         [fov 0.5135]
         [cx (vector3 (/ (* width fov) height) 0.0 0.0)]
         [cy (mul-v3 (normalize-v3 (cross-v3 cx gaze)) fov)])
    (camera eye cx cy gaze)))

; Main
(define (main)
  (begin
    (seed-rng)
    (let* ([width 1024]
           [height 768]
           [cam (default-camera width height)]
           [scene default-scene]
           [args (current-command-line-arguments)]
           [smax (samples args)]
           [Ls (loop-main scene cam height width smax)])
      (write-ppm width height (vector->list Ls) "rkt-smallpt.ppm"))))

(define (samples args)
  (if (> (vector-length args) 1)
      (/ (vector-ref args 1) 4.0)
      4))

(define (loop-main scene cam height width smax)
  (let ([Ls (make-vector (* width height) (vector3 0.0 0.0 0.0))])
    (loop-y scene cam 0 height width smax Ls)))
(define (loop-y scene cam y height width smax Ls0)
 (if (equal? y height)
     Ls0
     (begin
       (write (format "Rendering (~v spp) ~v%~n" (* smax 4) (~r (* 100.0 (/ y (- height 1))) #:precision 2)))
       (let ([Ls1 (loop-x scene cam y height 0 width smax Ls0)])
         (loop-y scene cam (+ y 1) height width smax Ls1)))))
(define (loop-x scene cam y height x width smax Ls0)
  (if (equal? x width)
      Ls0
      (let ([Ls1 (loop-sy scene cam y height x width 0 smax Ls0)])
        (loop-x scene cam y height (+ x 1) width smax Ls1))))
(define (loop-sy scene cam y height x width sy smax Ls0)
  (if (equal? sy 2)
      Ls0
      (let ([Ls1 (loop-sx scene cam y height x width sy 0 smax Ls0)])
        (loop-sy scene cam y height x width (+ sy 1) smax Ls1))))
(define (loop-sx scene cam y height x width sy sx smax Ls)
  (if (equal? sx 2)
      Ls
      (let* ([L0 (loop-s scene cam y height x width sy sx 0 smax (vector3 0.0 0.0 0.0))]
             [L1 (mul-v3 (clamp-v3 L0 0.0 1.0) 0.25)]
             [index (+ (* (- height 1 y) width) x)])
        (begin
             (vector-set! Ls index L1)
             (loop-sx scene cam y height x width sy (+ sx 1) smax Ls)))))
(define (loop-s scene cam y height x width sy sx s smax L0)
  (if (equal? s smax)
      L0
      (let ([L1 (do-s scene cam y height x width sy sx smax L0)])
        (loop-s scene cam y height x width sy sx (+ s 1) smax L1))))
(define (do-s scene cam y height x width sy sx smax L0)
  (let* ([df (lambda ()
               (let ([u (* 2.0 (uniform-float))])
                 (if (< u 1) (- (sqrt u) 1.0) (- 1.0 (sqrt (- 2.0 u))))))]
         [coefx (- (/ (+ (/ (+ sx 0.5 (df)) 2.0) x) width) 0.5)]
         [coefy (- (/ (+ (/ (+ sy 0.5 (df)) 2.0) y) height) 0.5)]
         [direction (add-v3 (add-v3 (mul-v3 (camera->cx cam) coefx) (mul-v3 (camera->cy cam) coefy)) (camera->gaze cam))]
         [ndirection (normalize-v3 direction)]
         [neye (add-v3 (camera->eye cam) (mul-v3 direction 130.0))]
         [L1 (radiance scene (ray neye ndirection epsilon-sphere positive-infinity 0))])
    (add-v3 L0 (div-v3 L1 smax))))
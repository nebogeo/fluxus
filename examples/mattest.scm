
;; a random collection of materials, presented on spheres

(clear)
(clear-lights)
(clear-colour (vector 1 1 1))

(show-axis 0)

(define (vrand)
  (vector (flxrnd) (flxrnd) (flxrnd)))

(define (sphere)
    (push)
    (opacity (flxrnd))
    (colour (vrand))    
    ;;(emissive (vrand))
    (specular (vrand))
    (ambient (vrand))
    (shinyness (* (flxrnd) 100))
    (scale (vector 0.5 0.5 0.5))
    (build-sphere 20 20)
    (pop))

(define (line n)
    (translate (vector 1 0 0))
    (sphere)
    (if (eq? 0 n)
        1
        (line (- n 1))))

(define (grid m n)
    (translate (vector 0 1 0))
    (push)
    (line n)
    (pop)
    (if (eq? 0 m)
        1
        (grid (- m 1) n)))

(define (make-grid)
  (push)
  (texture (load-texture "textures/test.png"))
  (scale (vector 12 12 12))
  (build-plane)
  (pop)

  (push)
  (translate (vector -6 -6 0))
  (grid 10 10)
  (pop))

(light-position (make-light 0) (vector 0 10 10))

(make-grid)


    
    

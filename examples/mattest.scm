
(define (sphere)
    (push)
    (opacity (flxrnd))
    (colour (vector (flxrnd) (flxrnd) (flxrnd)))    
    ;(emissive (vector (flxrnd) (flxrnd) (flxrnd)))    
    (specular (vector (flxrnd) (flxrnd) (flxrnd)))
    (ambient (vector (flxrnd) (flxrnd) (flxrnd)))
    (shinyness (*(flxrnd)100))
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

(show-axis 0)
(clear)
(clear-lights)
(clear-colour (vector 1 1 1))

(push)
(texture (load-texture "textures/test.png"))
(scale (vector 12 12 12))
(translate (vector 0.5 0.5 0))
(build-plane)
(pop)


(grid 10 10)


(define l1 (make-light 0))
(light-position l1 (vector 0 10 10))


    
    

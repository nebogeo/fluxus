; an example demonstrating some random material properties

; build a randomly shaded sphere
(define (sphere)
    (with-state
        (opacity (flxrnd))
        (colour (vector (flxrnd) (flxrnd) (flxrnd)))    
        ; emissive tends to take over somewhat, try it 
        ;(emissive (vector (flxrnd) (flxrnd) (flxrnd)))    
        (specular (vector (flxrnd) (flxrnd) (flxrnd)))
        (ambient (vector (flxrnd) (flxrnd) (flxrnd)))
        (shinyness (*(flxrnd)100))
        (scale (vector 0.5 0.5 0.5))
        ; tell fluxus to sort this to render transparency properly
        (hint-depth-sort)  
        (build-sphere 14 14)))

; build a line of spheres
(define (line n)
    (cond ((not (zero? n))
        (translate (vector 1 0 0))
        (sphere)
        (line (- n 1)))))

; build a grid of spheres
(define (grid m n)
    (cond ((not (zero? m))
        (translate (vector 0 1 0))
        (with-state (line n))
        (grid (- m 1) n))))

(clear)

; make a fixed light
(define l1 (make-light 'point 'free))
(light-position l1 (vector 0 10 10))

; put a plane in to show the transparency better
(with-state
    (texture (load-texture "test.png"))
    (scale (vector 12 12 12))
    (translate (vector 0.5 0.5 -0.05))
    (build-plane))

(grid 10 10)

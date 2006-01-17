; an example demonstrating some random material properties

; build a randomly shaded sphere
(define (sphere)
    (push)
    (opacity (flxrnd))
    (colour (vector (flxrnd) (flxrnd) (flxrnd)))    
	; emissive tends to take over somewhat, try it 
    ;(emissive (vector (flxrnd) (flxrnd) (flxrnd)))    
    (specular (vector (flxrnd) (flxrnd) (flxrnd)))
    (ambient (vector (flxrnd) (flxrnd) (flxrnd)))
    (shinyness (*(flxrnd)100))
    (scale (vector 0.5 0.5 0.5))
    (build-sphere 14 14)
    (pop))

; build a line of spheres
(define (line n)
    (translate (vector 1 0 0))
    (sphere)
    (if (zero? n)
        0
        (line (- n 1))))

; build a grid of spheres
(define (grid m n)
    (translate (vector 0 1 0))
    (push)
    (line n)
    (pop)
    (if (zero? m)
        0
        (grid (- m 1) n)))

(clear)
(clear-lights)
; make a fixed light
(define l1 (make-light 0))
(light-position l1 (vector 0 10 10))

; put a plane in to show the transparency better
(push)
(texture (load-texture "textures/test.png"))
(scale (vector 12 12 12))
(translate (vector 0.5 0.5 0))
(build-plane)
(pop)

(grid 10 10)

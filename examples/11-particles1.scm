; a particle example - not optimal, example 13 is a faster version of this, and it 
; renders 10 times more particles, but for clarity...

(define particle-count 1000)

; initialise the particles to have random initial velocity and colour
(define (init n)
    (pdata-set "vel" n (vmul (vsub (vector (flxrnd) (flxrnd) (flxrnd)) 
        (vector 0.5 0.5 0.5)) 0.1))
    (pdata-set "c" n (vector (flxrnd) (flxrnd) 1))
    (if (< n 0)
        0
        (init (- n 1))))

; every frame, subtract some "gravity" from the velocity, and then add the 
; velocity on to the position to animate the particle
(define (update n)
    (pdata-set "vel" n (vadd (pdata-get "vel" n) (vector 0 -0.001 0)))
    (pdata-set "p" n (vadd (pdata-get "p" n) (pdata-get "vel" n)))
    (if (< n 0)
        0
        (update (- n 1))))

(define (render)
    (grab ob)
    (update (pdata-size))
    (ungrab))

(clear)
(define ob (build-particles particle-count))

(grab ob)
; add the velocity pdata 
(pdata-add "vel" "v")
(init (pdata-size))
(ungrab)

(blur 0.1)
(every-frame "(render)")

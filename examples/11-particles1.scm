; a particle example - not optimal, example 13 is a faster version of
; this, and it renders 10 times more particles, but for clarity this
; is simpler...

(define particle-count 1000)

; util func to return a random vector
(define (rndvec)
    (vsub (vector (flxrnd) (flxrnd) (flxrnd)) 
        (vector 0.5 0.5 0.5)))

(clear)
(define particles (build-particles particle-count))

(with-primitive particles
    ; add the velocity pdata 
    (pdata-add "vel" "v")
    ; init all the velocities
    (pdata-map! (lambda (vel) (vmul (rndvec) 0.1)) "vel")
    ; init all the colours
    (pdata-map! (lambda (c) (vector (flxrnd) 0 0)) "c"))

(define (render)
    (with-primitive particles
        ; update the velocities
        (pdata-map! 
            (lambda (vel) (vadd vel (vector 0 -0.001 0))) 
            "vel")

        ; update the positions 
        ; (add the velocities onto the positions)
        (pdata-map! vadd "p" "vel")))


(blur 0.1)
(every-frame (render))

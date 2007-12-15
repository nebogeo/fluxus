(define particle-count 10000)

; init one particle
(define (init n)
    (pdata-set "p" n (vector 0 0 0))
    (pdata-set "vel" n (vmul (vsub (vector (flxrnd) (flxrnd) (flxrnd)) 
        (vector 0.5 0.5 0.5)) 0.1))
    (pdata-set "c" n (vector (flxrnd) (flxrnd) 1)))

; init some random particles
(define (initsome n)
    (cond ((not (zero? n))
        (init (random particle-count))
        (initsome (- n 1)))))

(define (animate)
    (with-primitive particles
        (initsome 100)    
        ; pdata-ops are a bit like simple pfuncs - 
        ; they are both similar experiments, pfuncs
        ; will be the way it works in the future
        (pdata-op "+" "vel" (vector 0 -0.002 0))
        (pdata-op "+" "p" "vel")))

(clear)
(show-fps 1)
(hint-none)
(hint-points)
(point-width 4)
(hint-anti-alias)

(define particles (build-particles particle-count))
(with-primitive particles (pdata-add "vel" "v"))
(blur 0.1)
(every-frame (animate))

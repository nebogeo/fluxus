(define particle-count 10000)

(define (init n)
    (pdata-set "p" n (vector 0 0 0))
    (pdata-set "vel" n (vmul (vsub (vector (flxrnd) (flxrnd) (flxrnd)) 
        (vector 0.5 0.5 0.5)) 0.1))
    (pdata-set "c" n (vector (flxrnd) (flxrnd) 1)))

(define (initsome n)
    (init (* particle-count (flxrnd)))
    (if (< n 0)
        0
        (initsome (- n 1))))

(define (initall n)
    (init n)
    (if (< n 0)
        0
        (initall (- n 1))))

(define (update)
    (pdata-op "+" "vel" (vector 0 -0.002 0))
    (pdata-op "+" "p" "vel"))

(define (render)
    (grab ob)
    (initsome 100)    
    (update)
    (ungrab))

(clear)
(show-fps 1)
;(hint-none)
;(hint-points)
(point-width 4)
(hint-anti-alias)

(define ob (build-particles particle-count))

(grab ob)
(pdata-add "vel" "v")
(initall (pdata-size))
(ungrab)
(blur 0.1)

(every-frame (render))

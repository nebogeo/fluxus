
(define particle-count 1000)

(define (init n)
    (pdata-set "vel" n (vmul (vsub (vector (flxrnd) (flxrnd) (flxrnd)) 
        (vector 0.5 0.5 0.5)) 0.1))
    (pdata-set "c" n (vector (flxrnd) (flxrnd) 1))
    (if (< n 0)
        0
        (init (- n 1))))

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
(show-fps 1)
(point-width 4)
(hint-anti-alias)

(define ob (build-particles particle-count))

(grab ob)
(pdata-add "vel" "v")
(init (pdata-size))
(ungrab)
(blur 0.1)

(every-frame "(render)")

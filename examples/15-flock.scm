
(clear)
(point-width 3)
(rotate (vector 0 90 0))
(define p (build-particles 100))

(define (init n)
    (pdata-set "p" n (vsub (vector 0 (flxrnd) (flxrnd)) (vector 0 0.5 0.5)))
    (let ((c (flxrnd)))
    (pdata-set "c" n (vector c c c)))
    (if (< n 1)
        0
        (init (- n 1))))

(define (update n)
    (let ((closest (pdata-op "closest" "p" n)))
        (let ((dir (vmul (vnormalise (vsub (pdata-get "p" n) closest)) 0.4))
              (centre (vmul (vnormalise (vsub (vector 0 0 0) (pdata-get "p" n))) 0.3)))
            (pdata-set "vel" n 
                (vmul (vnormalise 
                    (vadd (vmul (vadd dir centre) 0.001) (pdata-get "vel" n))) 0.02 ))))
          
    (if (< n 1)
        0
        (update (- n 1))))

(define (anim)
    (grab p)
    (update (pdata-size))
    (pdata-op "+" "p" "vel")
    (ungrab))

(grab p)
(pdata-add "vel" "v")
(init (pdata-size))
(ungrab)

(every-frame "(anim)")

(blur 0.1)


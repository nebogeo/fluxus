

(define t (vector 0 0 0))

(clear)
(push)
(texture (load-texture "green.png"))
(define ob (build-cube))
(pop)


(define (whack-texcoords n)
    ;(set! t (vector 0 (* 0.1 (sin (* (frame) 0.1))) 0))
    ;(display (pdata-get "t" n))
    ;(pdata-set "t" n (vadd (pdata-get "t" n) t))
    (if (< n 0)
        0
        (whack-texcoords (- n 1))))

(define (render)
    (grab ob)
    (whack-texcoords (pdata-size))
    ;(finalise)
    (ungrab))

(engine-callback "(render)")
 

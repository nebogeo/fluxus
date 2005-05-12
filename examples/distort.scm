


(define t (vector 0 0 0))

(clear)
(push)
(texture (load-texture "driller.png"))
(define ob (build-sphere))
(pop)


(define (whack-texcoords n)
    
    (set! t (vector 0 (* 0.1 (sin (time))) 0))
    (pdata-set "t" n (vadd (pdata-get "t" n) t))
    (if (< n 0)
        0
        (whack-texcoords (- n 1))))

(define (render)
    (grab ob)
    (whack-texcoords (pdata-size))
    (finalise)
    (ungrab))

(every-frame "(render)")
 

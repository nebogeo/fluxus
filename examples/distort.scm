


(define t (vector 0 0 0))

(clear)
(push)
(texture (load-texture "driller.png"))
(define ob (build-sphere 10 10))
(pop)


(define (whack-texcoords n)
    
    (set! t (vector (* 0.05 (cos (* 0.4 (+ n (time))))) (* 0.02 (sin (* 0.4 (time)))) 0))
    (pdata-set "t" n (vadd (pdata-get "t" n) t))
    (if (< n 0)
        0
        (whack-texcoords (- n 1))))

(define (render)
    (grab ob)
    (whack-texcoords (pdata-size))
    (finalise)
    (ungrab))

(start-framedump "test")

(every-frame "(render)")
 

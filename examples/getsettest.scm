

(define t (vector 0 0 0))

(clear)
(push)
(texture (load-texture "green.png"))
(define ob (build-cube))
(pop)


(define (whack-texcoords n)
    (set! t (vector 0 (* 0.001 (sin (* (frame) 0.001))) 0))
    (pdata-set "t" n (vadd (pdata-get "t" n) t))
    (if (< n 0)
        0
        (whack-texcoords (- n 1))))

(define (render)
    (grab ob)
    (whack-texcoords (pdata-size))
    (finalise)
    (ungrab)
    (render-instances 10))

(define (render-instances n)
    (translate (vector 1 0 0))
    (rotate (vector 0 45 0))
    (texture (load-texture "green.png"))
    (draw-instance ob)
    (if (eq? n 0)
        0
        (render-instances (- n 1))))

(engine-callback "(render)")
 

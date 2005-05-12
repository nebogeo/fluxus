
(clear)
(define ob (build-cube))
(grab ob)
(hide 1)
(ungrab)


(define (render)
    (translate (vector 2 0 0))
    (draw-instance ob))

(every-frame "(render)")

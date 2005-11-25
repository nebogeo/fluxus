(define (render)
    (push)
    (texture (load-texture "textures/green.png"))
    (draw-cube)
    (pop))
    
(every-frame "(render)")

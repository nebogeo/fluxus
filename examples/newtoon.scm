 

; a version of the toon shader script using the pop functions

(clear)
(define points '())

(hint-unlit)
(texture (load-texture "toon.png"))
(define ob (build-nurbs-sphere 5 8))
(clear-colour (vector 0 0 0.5))

(define (render)
    (grab ob)
    (pop-deform points pop-wave-deformer)
    (pop-shade pop-toon-shader)
    (ungrab))

(grab ob)
(set! points (pop-get-verts))
(ungrab)

(every-frame "(render)")
    




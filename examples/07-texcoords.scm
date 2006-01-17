


(clear)
(colour (vector 0.5 0.5 0.5))
(scale (vector 100 100 100))
(translate (vector -0.1 0 -0.1))
(specular (vector 0 0 0))
(clear-colour (vector 0 0 0))
(texture (load-texture "textures/red.png"))
(define p (build-nurbs-plane 5 5))

(define (deform n)
    (pdata-set "t" n (vadd (vector 0.0005 (* (sin n) 0.00) 0)
        (pdata-get "t" n)))
    (if (< n 1)
        0
        (deform (- n 1))))


(define (render)
    (grab p)
    (deform (pdata-size))
    (ungrab))

(every-frame "(render)")

(blur 0)

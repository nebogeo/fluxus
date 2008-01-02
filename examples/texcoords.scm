; an example of texture coordinate mangling

(clear)

(define p (with-state
    (colour (vector 0.5 0.5 0.5))
    (scale (vector 100 100 100))
    (rotate (vector 90 0 0))
    (translate (vector -0.4 -0.2 -0.4))
    (texture (load-texture "transp.png"))
    (build-nurbs-plane 5 5)))

(define (render)
    (with-primitive p
        (pdata-index-map!
            (lambda (n t)
                (vadd t (vector (* (sin n) 0.001) 
                                (* (cos n) 0.001) 0)))
            "t")))

(blur 0)
(every-frame (render))


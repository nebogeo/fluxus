; an example of texture coordinate mangling


; add a small vector on to the existing texture coordinates
(define (deform n)
    (pdata-set "t" n (vadd 
        (vector (* (sin n) 0.001) (* (cos n) 0.001) 0) ; small vector
        (pdata-get "t" n)))                   ; original coordinate
    (if (< n 1)
        0
        (deform (- n 1))))

(clear)

(push)
(colour (vector 0.5 0.5 0.5))
(scale (vector 100 100 100))
(rotate (vector 90 0 0))
(translate (vector -0.4 -0.2 -0.4))
(texture (load-texture "textures/transp.png"))
; make a new nurbs plane
(define p (build-nurbs-plane 5 5))
(pop)

(define (render)
    (grab p)
    (deform (pdata-size))
    (ungrab))

(blur 0)
(every-frame (render))


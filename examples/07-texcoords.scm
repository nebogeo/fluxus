; an example of texture coordinate mangling

; add a small vector on to the existing texture coordinates
(define (deform n)
    (pdata-set "t" n (vadd 
		(vector 0.0005 (* (sin n) 0.01) 0) ; small vector
        (pdata-get "t" n)))		           ; original coordinate
    (if (< n 1)
        0
        (deform (- n 1))))

(clear)

(push)
(colour (vector 0.5 0.5 0.5))
(scale (vector 100 100 100))
(translate (vector -0.1 0 -0.1))
(texture (load-texture "textures/scribblefont.png"))
; make a new nurbs plane
(define p (build-nurbs-plane 5 5))
(pop)

(define (render)
    (grab p)
    (deform (pdata-size))
    (ungrab))

(every-frame "(render)")


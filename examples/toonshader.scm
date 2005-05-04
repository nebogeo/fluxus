 


(clear)
(define v (vector 0 0 0))
(define i (vector 0 0 0))
(define t (vector 0 0 0))
(define points '())

(hint-unlit)
(texture (load-texture "toon.png"))
(shinyness 10)
(specular (vector 1 1 1))
(define ob (build-nurbs-sphere 20 40))

; reads the points into a list
(define (store n)
    (set! points (cons (pdata-get "p" n) points))
    (if (< n 0)
        0
        (store (- n 1))))

(define (deform n p)
    (set! v (vector (* 0.1 (sin (+ (*(frame)0.0) (*(vector-ref (car p) 1) 10.4)))) 0 0))
    (pdata-set "p" n (vadd v (car p)))
    (if (< n 0)
        0
        (deform (- n 1) (cdr p))))    
    
(define (toon n camerapos obpos)
    (set! v (vadd obpos (pdata-get "p" n))) ; vertex in worldspace 
    (set! i (vnormalise (vsub v camerapos))) ; incident direction    
    (pdata-set "t" n (vector (vdot i (pdata-get "n" n)) 0 0)) ; set s to the facing ratio    
    (if (< n 0)
        0
        (toon (- n 1) camerapos obpos)))    

(define (render)
    (grab ob)
    (deform (pdata-size) points)
    (recalc-normals)
    (toon (pdata-size)
        (vtransform (vector 0 0 0) (get-camera-transform))
        (vtransform (vector 0 0 0) (get-transform ob)))
    (finalise)
    (ungrab))

(grab ob)
(store (pdata-size))
(ungrab)

(engine-callback "(render)")
    




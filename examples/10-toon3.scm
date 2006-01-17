 
(show-fps 1)

(clear)
(define v (vector 0 0 0))
(define i (vector 0 0 0))
(define t (vector 0 0 0))

(clear-colour (vector 0 0 1))

;(hint-none)
(hint-unlit)
;(hint-wire)
(line-width 4)
(texture (load-texture "textures/toon.png"))
(shinyness 10)
(specular (vector 1 1 1))
(define ob (build-nurbs-sphere 10 20))
(grab ob)
(pdata-copy "p" "pref")
(ungrab)

(define (deform n)
    (set! v (vector (* 2 (sin (+ (time) (*(vector-ref (pdata-get "pref" n) 1) 10.4)))) 0 0))
    (set! v (vmul v (* (sin (time)) 0.5)))
    (pdata-set "p" n (vadd v (pdata-get "pref" n)))
    (if (< n 0)
        0
        (deform (- n 1))))    
    
(define (toon n camerapos obpos)
    (set! v (vadd obpos (pdata-get "p" n))) ; vertex in worldspace 
    (set! i (vnormalise (vsub v camerapos))) ; incident direction    
    (pdata-set "t" n (vector (vdot i (pdata-get "n" n)) 0 0)) ; set s to the facing ratio    
    (if (< n 0)
        0
        (toon (- n 1) camerapos obpos)))    

(define (render)
    (grab ob)
    (deform (pdata-size))
    (recalc-normals)
    (toon (pdata-size)
        (vtransform (vector 0 0 0) (get-camera-transform))
        (vtransform (vector 0 0 0) (get-transform)))
    (finalise)
    (ungrab)

    ;(render-instances -1)
)

(define (render-instances n)
    (translate (vector 1 0 0))
    (draw-instance ob)
    (if (< n 0)
        0
        (render-instances (- n 1))))


(every-frame "(render)")
    




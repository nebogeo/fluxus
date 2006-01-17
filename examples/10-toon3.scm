; this is another example of a different rendering technique, not toonshading style
; lighting, but similar approach which can be used to find edges

; instead of doing a lighting calculation, we figure out the viewing angle (the direction from the
; eye to the vertex on the object) and work out how much the normal of the vertex faces the viewer - 
; the facing ratio. we then use this to map the texture, so that the gradient wraps from the edges 
; to the middle of the object

(clear)
(clear-colour (vector 0 0 1))

(hint-unlit)
(line-width 4)
(texture (load-texture "textures/toon.png"))
(define ob (build-nurbs-sphere 10 20))
(grab ob)
; more reference geometry for the deformation
(pdata-copy "p" "pref")
(ungrab)

; some sinewave deformation with time
(define (deform n)
    (let ((v (vector (* 2 (sin (+ (time) (* (vector-ref (pdata-get "pref" n) 1) 10.4)))) 0 0)))
	    (set! v (vmul v (* (sin (time)) 0.5)))
	    (pdata-set "p" n (vadd v (pdata-get "pref" n))))
    (if (< n 0)
        0
        (deform (- n 1))))    

; this is the interesting part, the facing ratio is the dot product of the direction we are looking 
; at the vertex from, and the normal of the vertex - where all vectors are normalised. the complex bit
; is getting the incident direction, from the camera space transform (see below) and the vertex position
; in worldspace.
(define (toon n camerapos obpos)
    (let ((v (vadd obpos (pdata-get "p" n))))                           ; find the vertex in worldspace 
    	(let ((i (vnormalise (vsub v camerapos))))                      ; incident direction (normalised)
		    (pdata-set "t" n (vector (vdot i (pdata-get "n" n)) 0 0)))) ; set s to the facing ratio (i dot n) 
    (if (< n 0)
        0
        (toon (- n 1) camerapos obpos)))    

(define (render)
    (grab ob)
    (deform (pdata-size))
    (recalc-normals)
    (toon (pdata-size)
		; transforming a vector gets that vector "into" the space of the transform, so 0,0,0 in camera
		; space is the camera position...
        (vtransform (vector 0 0 0) (get-camera-transform)) ; gets the eye position
		
		; and 0,0,0 in object space is the object position
        (vtransform (vector 0 0 0) (get-transform))) ; gets the object position
    (ungrab))

(every-frame "(render)")

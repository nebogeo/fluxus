; the example demonstrates doing things with texture coordinates to
; achieve a different style of rendering

(clear)

; make a directional light, we will use in the 
; script as the light source
(define dirlight (vtransform (vector 0 1 0) 
                   (mrotate (vector 45 45 0))))

; this is a texture which define the lighting falloff as a gradient, 
; hard boundries give a toon shaded look
(texture (load-texture "gradient.png"))

; turn off normal gl lighting
(hint-unlit)

; the software lighting function, uses a dot product to calculate 
; the amount the normal faces into the light direction, 1 = full, 
; 0 = perpendicular, -1 means it's facing away

(define (toon-light)
    (pdata-map!
        (lambda (t n)
            ; set the s texture coordinate to the lighting amount
            (vector (max 0.1 (vdot n dirlight)) 0 0))     
       "t" "n"))

; deform the object so it's more interesting to light
(define (deform a s)
    (pdata-map!
        (lambda (p n)
            (vadd p (vmul n (* (sin (* (vector-ref p 1) s)) a))))
        "p" "n"))

; special deformation for the ground plane
(define (deform-plane)
    (pdata-map!
        (lambda (p n)
            (vadd p (vmul n 
              (* (sin (* (vdist (vector 5 0 5) p) 3)) 0.5))))
        "p" "n"))

; make the spheres and light them (we only need to calculate the lighting once
; if the object and the light are static, which is good, cos it's slow)
(define (make-spheres n)
    (cond ((not (zero? n))
        (with-state
            (colour (vector (flxrnd)(flxrnd)(flxrnd)))
            (translate (vmul (vector (flxrnd) 0.1 (flxrnd)) 8))
            (with-primitive (build-sphere 20 20)
                (deform (flxrnd) (* (flxrnd) 10))
                (recalc-normals 1)
                (toon-light)))
        (make-spheres (- n 1)))))

(make-spheres 5)

; make, deform and shade the ground plane
(with-state
    (scale (vector 10 10 10))
    (colour (vector (flxrnd)(flxrnd)(flxrnd)))
    (translate (vector 0 -1 0))
    (with-primitive (build-seg-plane 20 20)
        (apply-transform)
        (deform-plane)
        (recalc-normals 1)
        (toon-light)
        (rotate (vector -90 0 0))))

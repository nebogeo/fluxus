; the example demonstrates doing things with texture coordinates to
; achieve a different style of rendering

(clear)

; make a directional light, we will use in the script as the light source
(define dirlight (vtransform (vector 0 1 0) (mrotate (vector 45 45 0))))

; this is a texture which define the lighting falloff as a gradient, hard
; boundries give a toon shaded look
(texture (load-texture "textures/gradient.png"))

; turn off normal gl lighting
(hint-unlit)

; the software lighting function, uses a dot product to calculate the amount
; the normal faces into the light direction, 1 = full, 0 = perpendicular, 
; -1 means it's facing away
(define (toon-light n)
    (let ((lighting (vdot (pdata-get "n" n) dirlight)))
        (if (< lighting 0) (set! lighting 0.1))      ; reverse facing polys are nearly black
        (pdata-set "t" n (vector lighting 0 0)))     ; set the s texture coordinate
    (if (< n 1)
        0
        (toon-light (- n 1))))

; deform the object so it's more interesting to light
(define (deform n a s)
    (pdata-set "p" n (vadd (pdata-get "p" n) 
        (vmul (pdata-get "n" n)
            (* (sin (* (vector-ref (pdata-get "p" n) 1) s)) a))))
    (if (< n 1)
        0
        (deform (- n 1) a s)))

; special deformation for the ground plane
(define (deform-plane n)
    (pdata-set "p" n (vadd (pdata-get "p" n) 
        (vmul (pdata-get "n" n)
            (* (sin (* (vdist (vector 5 0 5) (pdata-get "p" n)) 3)) 0.5))))
    (if (< n 1)
        0
        (deform-plane (- n 1))))

; make the spheres and light them (we only need to calculate the lighting once
; if the object and the light are static, which is good, cos it's slow)
(define (make-spheres n)
    (push)
    (colour (vector (flxrnd)(flxrnd)(flxrnd)))
    (translate (vmul (vector (flxrnd) 0.1 (flxrnd)) 8))
    (let ((s (build-sphere 20 20)))
        (grab s)
        (deform (pdata-size) (flxrnd) (* (flxrnd) 10))
        (recalc-normals 1)
        (toon-light (pdata-size))
        (ungrab))
    (pop)
    (if (< n 1)
        0
        (make-spheres (- n 1))))

(make-spheres 5)

; make, deform and shade the ground plane
(push)
(scale (vector 10 10 10))
(colour (vector (flxrnd)(flxrnd)(flxrnd)))
(translate (vector 0 -1 0))
(let ((s (build-seg-plane 20 20)))
    (apply-transform s)
    (grab s)
    (deform-plane (pdata-size))
    (recalc-normals 1)
    (toon-light (pdata-size))
    (rotate (vector -90 0 0))
    (ungrab))
(pop)

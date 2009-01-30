; a digital dolly zoom/trombone shot/hitchcock effect
(clear)
(clear-colour (vector 0.2 0.5 1))
(translate (vector 0 0 -10))

; set up the lighting
(define l (make-light 'point 'free))
(light-diffuse 0 (vector 0 0 0))
(light-specular 0 (vector 0 0 0))
(light-position l (vector -100 100 100))
(light-diffuse l (vector 1 1 1))
(light-specular l (vector 1 1 1))
(shadow-light l)

; make a ground plane
(with-state
    (texture (load-texture "green.png"))
    (rotate (vector 90 0 0))
    (scale 50)
    (build-plane))

; make a bunch of toruses
    (for ((i (in-range 0 20)))
        (with-state
            (hint-cast-shadow)
            (specular (vector 1 1 1))
            (shinyness 50)
            (colour (rndvec))
            (rotate (vector 0 (* 360 (rndf)) 0))
            (translate (vmul (vector (crndf) 0.1 (crndf)) 10))
            (build-torus 0.2 1 10 10)))

    (for ((i (in-range 0 20)))
        (with-state
            (hint-cast-shadow)
            (specular (vector 1 1 1))
            (shinyness 50)
            (colour (rndvec))
            (scale (vector 1 (* 10 (rndf)) 1))
            (translate (vmul (vector (crndf) 0.05 (- (rndf))) 10))
            (rotate (vector 0 (* 360 (rndf)) 0))
            (build-cube)))

; make the subject
(with-state
    (hint-cast-shadow)
    (texture (load-texture "test.png"))
    (translate (vector 0 2 0))
    (shinyness 50)
    (specular (vector 1 1 1))
    (build-sphere 20 20))

; move and simultaneously change the angle of the camera
(define (animate)
    (let ((p (+ 0.5 (* (sin (time)) 0.4)))
          (c (+ 0.2 (* (- (sin (time))) 0.6))))
        (set-camera-transform (mtranslate (vector 0 -2 (* 10 c))))
        (clip p 1000)))

(every-frame (animate))
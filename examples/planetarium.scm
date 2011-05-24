; The test script I used for projection at the Plymouth 
; University Immersive Vision Theatre

(require fluxus-018/planetarium)

(clear)

; use 3 cameras, a fov of 180 degrees and a target size of 2048x2048
(define dome (dome-build 3 180 2048))

; make a scene from some cubes
(with-pixels-renderer (dome-pixels)
    (with-state
        (translate (vector -10 -1 6))
        (rotate (vector 0 90 0))
        (for ((x (in-range 0 10)))
            (for ((y (in-range 0 10)))
                (with-state
                    (colour (rndvec))
                    (translate (vmul (vector x 0 y) 2))
                    (build-cube))))))

; draw the dome in wireframe to make sure the dome geometry 
; corresponds to the real dome
(with-primitive dome
    (wire-colour 0.4)
    (hint-wire))

; set the view of the dome
(dome-setup-main-camera 1400 1050)

; do some animation
(every-frame 
    (with-pixels-renderer (dome-pixels)
        (with-state
            (translate (vector 0 1 -30))
            (rotate (vector (* (time) 180) 0 0))
            (draw-torus))))

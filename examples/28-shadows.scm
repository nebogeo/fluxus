; test the shadow generation

(define (scene x y)
    (define (row x)
        (translate (vector 1 0 0))
        (push)
        (let ((s (* 0.4 (flxrnd))))
        (scale (vector s s s)))
        (translate (vector 0 (* 0.4 (flxrnd)) 0))
        (rotate (vector 90 0 0))
        (cond 
            ((zero? (random 2)) 
                (scale (vector 1 1 5))
                (build-cube))
            (else (build-sphere 8 8)))
        (pop)
        (if (zero? x)
            0
            (row (- x 1))))
    (translate (vector 0 0 1))
    (push)
    (row x)
    (pop)
    (if (zero? y)
        0
        (scene x (- y 1))))


(clear)
(clear-colour (vector 0 0.5 1))

; turn the camera light down
(light-diffuse 0 (vector 0.4 0.4 0.4))

; make a new light for the shadowing
(define l (make-light "point" "free"))
(light-diffuse l (vector 0 1 1))
(light-position l (vector 50 20 50))
(shadow-light l)

; set this to 1 to view the shadow volume
(shadow-debug 0)

(push)
; tells fluxus that subsequent objects cast a shadow
(hint-cast-shadow)
(translate (vector -5 0 -5))
(scene 10 10)
(pop)

; make the floor
(push)
(translate (vector 0 0 0))
(rotate (vector 90 0 0))
(scale (vector 20 20 20)) 
(build-plane)
(pop)

; swing the light around in a disturbing manner...
(define (render)
    (light-position l (vmul (vector (sin (time)) 0.2 (cos (time))) 50)))

(every-frame (render))
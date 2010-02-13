; press space

(clear)

(light-diffuse 0 (vector 0.4 0.4 0.4))
(define l (make-light 'point 'free))
(light-diffuse l (vector 1 1 1))
(light-position l (vector 20 5 30))
(shadow-light l)
(shadow-debug 0)

(with-state
    (translate (vector 0 -1 0))
    (rotate (vector 90 0 0))
    (scale (vector 20 20 20)) 
    (build-plane))

(hint-cast-shadow)

(collisions 1)
(ground-plane (vector 0 1 0) -1)

(define (render)
    (cond 
        ((key-pressed " ")
            (with-state
                (translate (vector 0 3 0))
                (scale (vector (flxrnd) (flxrnd) (flxrnd)))
                (active-box (build-cube))))))

(every-frame (render))

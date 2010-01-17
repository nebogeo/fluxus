(clear)
(collisions 1)

(light-diffuse 0 (vector 0 0 0))
(let ((l (make-light 'point 'free)))
 (light-position l (vector 10 100 10))
 (light-diffuse l (vector 1 1 1))
 (light-ambient l (vector 0.2 0.2 0.2))
 (shadow-light l))

(define (new-widget)
    (let ((m (load-primitive "widget.obj")))
        (with-primitive m
            (hint-cast-shadow)
            (scale 10)
            (translate (vmul (vector (crndf) 0 (crndf)) 2))
            (rotate (vector 90 0 (* 360 (rndf))))
            (poly-convert-to-indexed))
        (passive-mesh m) ))

(define (new-bot x)
    (let ((m (load-primitive "bot.obj")))
        (with-primitive m
            (hint-cast-shadow)
            (translate (vmul (vector (- x 5) 4 0) 2))
            (recalc-normals 0)
            (poly-convert-to-indexed))
        (active-mesh m)))

(gravity (vector 0 -1 0))
(ground-plane (vector 0 1 0) 0)
(with-state
    (rotate (vector 90 0 0))
    (scale 50)
    (build-plane))

(for ((i (in-range 0 10)))
    (new-widget))

(for ((i (in-range 0 10)))
    (new-bot i))
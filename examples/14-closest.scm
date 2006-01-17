(clear)
(clear-colour (vector 0 0.5 0.5))
(push)
(scale (vector 2 2 2))
(hint-none)
(hint-wire)
(define s (build-sphere 50 50))
(apply s)
(pop)
(push)
(translate (vector 2 0 0))
(scale (vector 0.1 0.1 0.1))
(define o (build-cube))
(pop)

(define (stick pos)
    (grab s)
    (let ((close (pdata-op "closest" "p" pos)))
        (ungrab)
        (push)
        (translate close)
        (scale (vector 0.1 0.1 0.1))
        (build-cube)
        (pop)))


(define (update)
    (grab o)
    (identity)
    (translate (vmul (vector (sin (time)) (sin (* (time) 0.4)) (cos
(time))) 2))
    (scale (vector 0.1 0.1 0.1))
    (let ((pos (vtransform (vector 0 0 0) (get-transform))))
    (ungrab)

    (stick pos)))

(every-frame "(update)")





(clear)
(define p (build-pixels 256 256))

(define q (with-pixels-renderer p
        (clear-colour (vector 1 1 1))
        (scale 2)
        (colour (vector 0 0 1))
        (build-torus 1 2 10 10)))

(define o (with-state
        (rotate (vector 180 0 0))
        (translate (vector 2 -0.5 0))
        (texture (pixels->texture p))
        (build-torus 0.2 0.4 20 20)))

(every-frame
    (with-pixels-renderer p
        (with-primitive q
            (rotate (vector 1 0.2 0)))))

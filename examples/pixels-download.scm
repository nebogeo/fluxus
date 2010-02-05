(clear)

(define p (build-pixels 256 256 #t))

(define q (with-pixels-renderer p
        (clear-colour (vector 1 1 1))
        (scale 3)
        (colour (vector 0 0 1))
        (build-torus 0.2 2 10 20)))

(define i (with-state
        (translate (vector 0.5 0.5 0))
        (scale 0.2)
        (build-cube)))

(every-frame
    (begin
    (with-primitive p 
        (pixels-download)
        ; paint the cube with the colour of the pixel underneath it
        (let ((c (pdata-ref "c" (pixels-index (vector 0.5 0.5 0)))))
            (with-primitive i (colour c))))
    (with-pixels-renderer p
        (with-primitive q
            (rotate (vector 1 0.2 0))))))

; test some of the poly evaluation code with as many poly 
; primitive types as it supports - note the support of these
; is not exhaustive yet...

; test poly-for-each-face
; puts a cube at the centre of each face
(define (face-middle)
    (poly-for-each-face
        (lambda (indices)
            (let ((mid (vdiv (foldl
                    (lambda (i r)
                        (vadd r (pdata-get "p" i)))
                    (vector 0 0 0)
                    indices) 
                    (length indices))))
            (with-state
                (hint-none)
                (hint-solid)
                (colour (vector 1 0 0))
                (translate mid)
                (scale (vector 0.1 0.1 0.1))
                (build-cube))))))

(clear)
(backfacecull 0)
(hint-unlit)
(hint-wire)
(wire-colour (vector 0 0 0))
(colour (vector 1 1 0))

(with-primitive (build-polygons 6 'triangle-strip)
    (pdata-set! "p" 0 (vector 0 0 0))
    (pdata-set! "p" 1 (vector 0 1 0))
    (pdata-set! "p" 2 (vector 1 0 0))
    (pdata-set! "p" 3 (vector 1 1 0))
    (pdata-set! "p" 4 (vector 2 0 0))
    (pdata-set! "p" 5 (vector 2 1 0))
    (face-middle))

(translate (vector 0 1.1 0))
(with-primitive (build-polygons 6 'triangle-list)
    (pdata-set! "p" 0 (vector 0 0 0))
    (pdata-set! "p" 1 (vector 0 1 0))
    (pdata-set! "p" 2 (vector 1 0 0))
    (pdata-set! "p" 3 (vector 1 1 0))
    (pdata-set! "p" 4 (vector 2 0 0))
    (pdata-set! "p" 5 (vector 2 1 0))
    (face-middle))

(translate (vector 0 1.1 0))
(with-primitive (build-polygons 8 'quad-list)
    (pdata-set! "p" 0 (vector 0 0 0))
    (pdata-set! "p" 1 (vector 0 1 0))
    (pdata-set! "p" 2 (vector 1 1 0))
    (pdata-set! "p" 3 (vector 1 0 0))
    (pdata-set! "p" 4 (vector 2 0 0))
    (pdata-set! "p" 5 (vector 2 1 0))
    (pdata-set! "p" 6 (vector 3 1 0))
    (pdata-set! "p" 7 (vector 3 0 0))
    (face-middle))

(translate (vector 0 1.1 0))
(with-primitive (build-polygons 7 'triangle-fan)
    (pdata-set! "p" 0 (vector 0 0 0))
    (pdata-set! "p" 1 (vector 0 1 0))
    (pdata-set! "p" 2 (vector 0.2 1 0))
    (pdata-set! "p" 3 (vector 0.4 1 0))
    (pdata-set! "p" 4 (vector 0.6 1 0))
    (pdata-set! "p" 5 (vector 0.8 1 0))
    (pdata-set! "p" 6 (vector 1 1 0))
    (face-middle))

(translate (vector 0 1.1 0))
(with-primitive (build-polygons 7 'polygon)
    (pdata-set! "p" 0 (vector 0 0 0))
    (pdata-set! "p" 1 (vector 0 1 0))
    (pdata-set! "p" 2 (vector 0.2 1 0))
    (pdata-set! "p" 3 (vector 0.4 1 0))
    (pdata-set! "p" 4 (vector 0.6 1 0))
    (pdata-set! "p" 5 (vector 0.8 1 0))
    (pdata-set! "p" 6 (vector 1 1 0))
    (face-middle))

(translate (vector 4 -4.5 0))
(colour (vector 0 0 1))

(translate (vector 0 1.1 0))
(with-primitive (with-state (scale 1) (build-sphere 6 4))
    (poly-convert-to-indexed)
    (face-middle))

(translate (vector 0 2.1 0))
(with-primitive (build-torus 0.3 0.5 8 10)
    (poly-convert-to-indexed)
    (face-middle))

; test the poly-for-each-triangle
(translate (vector -1 1.1 0))
(with-primitive (build-polygons 8 'quad-list)
    (pdata-set! "p" 0 (vector 0 0 0))
    (pdata-set! "p" 1 (vector 0 1 0))
    (pdata-set! "p" 2 (vector 1 1 0))
    (pdata-set! "p" 3 (vector 1 0 0))
    (pdata-set! "p" 4 (vector 2 0 0))
    (pdata-set! "p" 5 (vector 2 1 0))
    (pdata-set! "p" 6 (vector 3 1 0))
    (pdata-set! "p" 7 (vector 3 0 0))
    (poly-for-each-triangle
        (lambda (indices)
            (let ((mid (vdiv (vadd (vadd 
                (pdata-get "p" (list-ref indices 0))
                (pdata-get "p" (list-ref indices 1)))
                (pdata-get "p" (list-ref indices 2))) 3)))
            (with-state
                (colour (vector 1 0 0))
                (translate mid)
                (scale 0.1)
                (build-cube))))))

; test poly-for-each-tri-sample
(translate (vector 0 1.1 0))
(colour (vector 0 1 1))
(with-primitive (build-polygons 6 'triangle-list)
    (pdata-set! "p" 0 (vector 0 0 0))
    (pdata-set! "p" 1 (vector 0 1 0))
    (pdata-set! "p" 2 (vector 1 0 0))
    (pdata-set! "p" 3 (vector 1 1 0))
    (pdata-set! "p" 4 (vector 2 0 0))
    (pdata-set! "p" 5 (vector 2 1 0))
    (poly-for-each-tri-sample
        (lambda (indices bary)
            (let ((pos (vadd (vadd 
                (vmul (pdata-get "p" (list-ref indices 0)) (vx bary))
                (vmul (pdata-get "p" (list-ref indices 1)) (vy bary)))
                (vmul (pdata-get "p" (list-ref indices 2)) (vz bary)))))
            (with-state
                (colour (vector 0 0 0))
                (translate pos)
                (scale 0.01)
                (build-cube))))
        100))












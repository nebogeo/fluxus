; click on the donuts!
(clear)

(define (make-donuts n)
    (when (not (zero? n))
        (with-state
            (translate (vmul (srndvec) 5))
            (scale 0.1)
            (build-torus 1 2 12 12))
        (make-donuts (- n 1))))

(make-donuts 10)

(every-frame
    (when (mouse-button 1)
        (let ((s (select (mouse-x) (mouse-y) 2)))
            (when (not (zero? s))
                (with-primitive s
                    (colour (rndvec)))))))

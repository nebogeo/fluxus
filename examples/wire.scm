
(clear)
(hint-none)
(hint-wire)
(hint-vertcols)
(line-width 2)
(define my-sphere (build-sphere 20 20))

(define (animate)
    (with-primitive my-sphere
        (pdata-index-map!
            (lambda (n c)
                (vector (sin (+ n (time))) 
                        (cos (+ n (* 2.3 (time))))
                        0))
            "c")))

(every-frame (animate))


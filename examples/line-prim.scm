
(clear)
(desiredfps 100)
(texture (load-texture "test.png"))
(hint-vertcols)
(define my-line (build-line 130))

; set the width of every vert to 0.5
(with-primitive my-line
    (pdata-map! (lambda (w) 0.5) "w"))

(define (animate)
    (let ((a (* 0.3 (sin (* 0.01 (time))))))
        (with-primitive my-line
            (pdata-index-map!
                (lambda (n p)
                    (vmul (vector (sin (* 0.1 n)) 
                                  (cos (* 0.1 n)) 0) 
                        (* 10 (sin (+ (cos (* n a)) 
                                      (* 0.5 (time)))))))
                 "p"))))

(every-frame (animate))
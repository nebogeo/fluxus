
(clear)

(define (build n)
    (translate (vector 1.1 0 0))
    (build-cube)
    (if (< n 0)
        0
        (build (- n 1))))

(define (render)
    (if (mouse-button 1)
        (begin
        (grab (mouse-over))
        (scale (vector 1 1.1 1))
        (ungrab)))
    (if (mouse-button 2)
        (begin
        (grab (mouse-over))
        (scale (vector 1 0.9 1))
        (ungrab))))


(build 10)
(every-frame "(render)")


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
        (rotate (vector 1 0 0))
        (colour (vector (flxrnd)(flxrnd)(flxrnd)))
        (ungrab))))


(build 10)
(every-frame "(render)")

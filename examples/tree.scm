(define (tree d)
    (push)
    (rotate #(0 30 0))
    (translate #(0 0.6 0))
    (scale #(0.8 0.8 0.8))
    (push)
    (scale #(0.2 1 0.2))
    (draw-cube)
    (pop)
    (if (eq? 0 d)
        1
        (begin    (rotate #(0 0 45))
            (tree (- d 1))
            (rotate #(0 0 -90))
            (tree (- d 1))))
    (pop))

(show-axis 1)
(clear)

(colour #(0.5 0.5 0.5))
(define (loop) (tree 8))

(every-frame "(loop)")

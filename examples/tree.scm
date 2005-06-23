(define (tree d)
    (push)
    (rotate (vector 0 30 0))
    (translate (vector 0 0.6 0))
    (scale (vector 0.8 0.8 0.8))
    (push)
    (scale (vector 0.2 1 0.2))
    (draw-cube)
    (pop)
    (if (eq? 0 d)
        1
        (begin    (rotate (vector 0 0 45))
            (tree (- d 1))
            (rotate (vector 0 0 -90))
            (tree (- d 1))))
    (pop))

(show-axis 1)
(clear)

(colour (vector 0.5 0.5 0.5))
(define (loop) (tree 8))

(every-frame "(loop)")

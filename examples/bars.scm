
(define (bar c)
    (push)
    (colour (vector 0 0 (gh c)))
    (translate (vector c 0 0))
    (scale (vector 1 (* 5 (gh c)) 1))
    (draw-cube)
    (pop))

(define (bars c)
    (bar c)
    (if (eq? c 16)
        0
        (bars (+ c 1))))


(define (render)
    (bars 0))

(engine-callback "(render)")
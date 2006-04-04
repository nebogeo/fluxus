
; draw a bar, using c as the harmonic to look up
(define (bar c)
    (push)
    (colour (vector 1 0 (gh c)))
    (translate (vector c 0 0))
    (scale (vector 1 (+ 0.1 (* 5 (gh c))) 1))
    (draw-cube)
    (pop))

; draw 16 bars, loop recursively
(define (bars c)
    (bar c)
    (if (eq? c 16)
        0
        (bars (+ c 1))))


(define (render)
    (bars 0))

(every-frame (render))


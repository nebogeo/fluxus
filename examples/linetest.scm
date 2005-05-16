
(define point (vector 0 0 0))
(define width 0.1)

(define (draw n)
    (let ((newpoint (vector (*(flxrnd) 10)(*(flxrnd) 10)(*(flxrnd) 10)))
          (newwidth (*(flxrnd)0.1)))
        (build-line point width newpoint newwidth)
        (set! point newpoint)
        (set! width newwidth))
    (if (< n 0)
        0
        (draw (- n 1))))


(clear)
(show-axis 1)
(hint-unlit)
(colour (vector 1 1 1))
(blur 0.1)

(draw 100)



(define point (vector 0 0 0))
(define width 0.1)

(define (draw)
	(let ((newpoint (vector (*(flxrnd) 10)(*(flxrnd) 10)(*(flxrnd) 10)))
		  (newwidth (*(flxrnd)0.1)))
		(build_line point width newpoint newwidth)
		(set! point newpoint)
		(set! width newwidth)))


(clear)
(show_axis 1)
(colour (vector 1 1 1))
(blur 0.001)

(draw)

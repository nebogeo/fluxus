(define x1 0)
(define y1 0)
(define z1 90)
(define x2 0)
(define y2 0)
(define z2 -180)
(define depth 7)

(define (tree d)
	(push)
	(rotate (vector 0 (*(gh d)50) 0))
	(colour (vector (gh d) (gh (+ d 4)) (gh (+ d 3))))
	(opacity (* 2 (gh d)))
	(shinyness 10)
	(translate (vector 0 0.6 0))
	(scale (vector 0.8 (+(*(gh (+ d 5))0.1)0.8) 0.8))
	(rotate (vector 0 0 (*(gh (- d 3))50)))
	(push)
		(scale #(0.2 0.3 0.2))
		(draw_sphere)
	(pop)
	(if (eq? depth d)
		1
		(begin	(rotate (vector x1 y1 (+ d z1)))
			(tree (+ d 1))
			(rotate (vector x2 y2 z2))
			(tree (+ d 1))))
	(pop))

(define (draw_loop)
	(colour #(0.1 0.1 0.1))
	(opacity 0.5)
	(tree 0))

(clear)
(blur 0.001)
(show_axis 0)
(engine_callback "(draw_loop)")
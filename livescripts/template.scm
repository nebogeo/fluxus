(define col 1)

(define (draw n)
	(push)
	;(colour (vector (* col (gh (+ n 1))) 
	;				(* col (gh n) )
	;				(* col (gh (- n 5)))))
	;(scale (vector 1(* 5 (gh n) )1))
	(draw-cube)
	(pop)
	(if (eq? n 0)
		1
		(draw (- n 1))))

(define (render)
	(draw 10))
(clear)
(engine-callback "(render)")


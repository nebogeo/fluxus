(define col 2)
(define sc 10)

(define (draw n)
	(opacity 1)
	(translate (vector 1 0.2 0))
	(push)
	(colour (vector (* col (gh (+ n 4))) 
					(* col (gh n) )
					(* col (gh (- n 3)))))
	(scale (vector (* 1 (gh (+ n 1))) 
					(* 1 (gh n) )
					(* sc (gh n))))
	(draw-cube)
	(pop)
	(if (eq? n 0)
		1
		(begin 
		
		(draw (- n 1))
		
		(rotate (vector 0 5 0))
		
		(draw (- n 1))
		
		)))

(define (render)
	(draw 7))
(clear)
(every-frame "(render)")
(blur 1)

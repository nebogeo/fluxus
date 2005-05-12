(define (render)
	(push)
	(texture (load-texture "green.png"))
	(draw-cube)
	(pop))
	
(every-frame "(render)")

(define (sphere)
	(push)
	(colour (vector (flxrnd) (flxrnd) (flxrnd)))	
	(specular (vector (flxrnd) (flxrnd) (flxrnd)))
	(ambient (vector (flxrnd) (flxrnd) (flxrnd)))
	(shinyness (*(flxrnd)100))
	(scale (vector 0.5 0.5 0.5))
	(let ((ob (build_sphere 8 10)))	
		(active_sphere ob))
	(pop))

(define (line n)
	(translate (vector (* 0.01 (flxrnd)) (+ 1 (* 0.01 (flxrnd))) (* 0.001 (flxrnd)) ))
	(sphere)
	(if (eq? 0 n)
		1
		(line (- n 1))))

(clear)
(collisions 1)
(ground_plane (vector 0 1 0) 0)
(show_axis 1)
(clear_colour (vector 0.5 0.6 0.9))
(line 10)


	

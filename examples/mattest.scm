(define (sphere)
	(push)
	(colour (vector (flxrnd) (flxrnd) (flxrnd)))	
	;(emissive (vector (flxrnd) (flxrnd) (flxrnd)))	
	(specular (vector (flxrnd) (flxrnd) (flxrnd)))
	(ambient (vector (flxrnd) (flxrnd) (flxrnd)))
	(shinyness (*(flxrnd)100))
	(scale (vector 0.5 0.5 0.5))
	(build_sphere 10 20)
	(pop))

(define (line n)
	(translate (vector 1 0 0))
	(sphere)
	(if (eq? 0 n)
		1
		(line (- n 1))))

(define (grid m n)
	(translate (vector 0 1 0))
	(push)
	(line n)
	(pop)
	(if (eq? 0 m)
		1
		(grid (- m 1) n)))

(show_axis 1)
(clear)
(grid 10 10)



	
	

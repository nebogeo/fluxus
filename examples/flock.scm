(clear)
(make_lifeforms "life")

(define (spawn) 
	(push)
	(colour (vector (flxrnd)(flxrnd)(flxrnd)))
	(translate (vector (*(flxrnd)5) 0 (*(flxrnd)5)))
	(add_lifeform "life" (build_cube))
	(pop))

(lifeform_avoidance "life" 5)
(lifeform_flockcentering "life" 0.5)
(lifeform_inertia "life" 0.998)
(lifeform_maxspeed "life" 0.1)
(lifeform_scenecentering "life" 0.5)

(blur 0.01)

(spawn)(spawn)(spawn)(spawn)(spawn)(spawn)
(spawn)(spawn)(spawn)(spawn)(spawn)(spawn)
(spawn)(spawn)(spawn)(spawn)(spawn)(spawn)
(spawn)(spawn)(spawn)(spawn)(spawn)(spawn)

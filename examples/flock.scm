(clear)
(make-lifeforms "life")

(define (spawn) 
	(push)
	(colour (vector (flxrnd)(flxrnd)(flxrnd)))
	(translate (vector (*(flxrnd)5) 0 (*(flxrnd)5)))
	(add-lifeform "life" (build-cube))
	(pop))

(lifeform-avoidance "life" 5)
(lifeform-flockcentering "life" 0.5)
(lifeform-inertia "life" 0.998)
(lifeform-maxspeed "life" 0.1)
(lifeform-scenecentering "life" 0.5)

(blur 0.01)

(spawn)(spawn)(spawn)(spawn)(spawn)(spawn)
(spawn)(spawn)(spawn)(spawn)(spawn)(spawn)
(spawn)(spawn)(spawn)(spawn)(spawn)(spawn)
(spawn)(spawn)(spawn)(spawn)(spawn)(spawn)

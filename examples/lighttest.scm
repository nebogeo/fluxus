(clear)
(clear-lights)

(push)
(specular (vector 1 1 1))
(shinyness 50)
(scale (vector 5 5 5))
(texture (load-texture "pawfal.png"))
(build-sphere 80 80)
(pop)

(define l1 (make-light 0))
(light-diffuse l1 (vector 1 0 0))
(light-position l1 (vector 10 0 0))

(define l2 (make-light 0))
(light-diffuse l2 (vector 0 1 0))
(light-position l2 (vector 0 10 0))

(define l3 (make-light 0))
(light-diffuse l3 (vector 0 0 1))
(light-position l3 (vector 0 0 10))
(clear)
(clear_lights)

(push)
(specular (vector 1 1 1))
(shinyness 50)
(scale (vector 5 5 5))
(texture (load_texture "pawfal.png"))
(build_sphere 80 80)
(pop)

(define l1 (make_light))
(light_diffuse l1 (vector 1 0 0))
(light_position l1 (vector 10 0 0))

(define l2 (make_light))
(light_diffuse l2 (vector 0 1 0))
(light_position l2 (vector 0 10 0))

(define l3 (make_light))
(light_diffuse l3 (vector 0 0 1))
(light_position l3 (vector 0 0 10))
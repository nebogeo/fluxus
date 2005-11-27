(clear)
(clear-lights)
(clear-colour (vector 1 1 1))

(push)
(specular (vector 1 1 1))
(shinyness 50)

(define (s n)
    (push)
    (translate (vmul (vector (sin n) 0 (cos n)) (* n 0.1)))
    (build-sphere 20 20)
    (pop)
    (if (< n 1)
        0
        (s (- n 1))))

(s 60)

(pop)

(define l1 (make-light 0))
(light-diffuse l1 (vector 1 0 0))
(light-position l1 (vector 10 10 0))

(define l2 (make-light 0))
(light-diffuse l2 (vector 0 1 0))
(light-position l2 (vector 0 10 0))

(define l3 (make-light 0))
(light-diffuse l3 (vector 0 0 1))
(light-position l3 (vector 0 0 10))

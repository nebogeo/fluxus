
(require fluxus-018/frisbee)
(clear)

(define (new-mushroom pos lst)
  (let ((age (- milliseconds (value-now milliseconds))))
  (append
   (list
      (cond ((< age 8000)
         (object 
               #:colour (vec3 (+ 0.9 (* 0.1 (rndf))) (+ 0.5 (* (rndf) 0.5)) 0.8)
               #:shape "mushroom.obj"
               #:translate pos
               #:scale (vmul (vec3 0.5 0.5 0.5) (+ 0.1 (* age 0.0001))))))
      (if (> age 500) (new-mushroom (vadd pos (vec3 (crndf) 0 (crndf))) '())))
    lst)))
                 
(scene
 (list
  (new-mushroom (vec3 0 0 0) '())
  (object 
        #:shape 'plane 
        #:rotate (vec3 90 0 0)
        #:colour (vec3 0.6 0.9 0.7)
        #:scale (vec3 50 50 1))))





















(clear-colour (vector 1 1 1))
(define l (make-light 'point 'free))
(light-position l (vector 1000 300 200))
(light-diffuse l (vector 1 1 1))
(define l (make-light 'point 'free))
(light-position l (vector 1000 300 200))
(light-diffuse l (vector 1 1 1))
(light-diffuse 0 (vector 0.5 0.5 0.5))
(shadow-light l)
(hint-cast-shadow)

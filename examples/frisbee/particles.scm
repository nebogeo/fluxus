(require fluxus-018/frisbee)

(scene
 (list
  (particles
   #:spread 50
   #:rotate (vec3 0 0 (integral 0.1))
   #:colour (vec3 
             (+ 0.5 (* 0.5 (sin (integral 0.01))))
             (+ 0.5 (* 0.5 (sin (integral 0.001))))
             1)
   #:translate (vec3 0 -2 0)
   
   #:rate 5)
  
  (particles
   #:spread 360
   #:colour (vec3 
             1
             (+ 0.5 (* 0.5 (sin (integral 0.01))))
             (+ 0.5 (* 0.5 (sin (integral 0.001)))))
   #:translate (vec3 0 2 0)
   
   #:rate 5)))

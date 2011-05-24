(require fluxus-018/frisbee)

(define player-pos
  (vec3 (key-control-b #\d #\a 0.01)
        (+ (key-control-b #\w #\s 0.01) 5) 0))

(scene 
 (list

  (object 
   #:translate player-pos
   #:colour (vec3 0 0 1))
  
  (factory
      (lambda (e)
          (let ((pos (vsub (vec3-integral (vec3 0 0.002 0)) (vec3 0 3 0))))
            (if (> (vdist pos player-pos) 2)
                (object 
                 #:shape "alien.obj"
                 #:scale (vec3 0.3 0.3 0.3)
                 #:colour (vec3 1 0 0)
                 #:translate pos))))
      (metro 1) 4)))

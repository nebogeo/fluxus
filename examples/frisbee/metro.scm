(require fluxus-018/frisbee)

(scene
 (list  
  (factory
   (lambda (e)
     (object #:translate (vec3-integral (vec3 0.0012 0 0))))
   (metro 1) 5)))

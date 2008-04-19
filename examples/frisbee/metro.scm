(require (lib "frisbee.ss" "fluxus-0.15"))

(scene
 (list  
  (factory
   (lambda (e)
     (object #:translate (vec3-integral (vec3 0.012 0 0))))
   (metro 0.1) 5)))

(show-fps 1)

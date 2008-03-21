(require (lib "frisbee.ss" "fluxus-0.15"))

(define player-pos
  (vec3 (key-control-b #\d #\a 0.01) -7 0))

(define fire-time-e (key-time-e #\ ))

(define bullets 
  (factory
   (lambda (t)
     (if (< clock (+ t 10000))
         (object
          #:shape 'sphere
          #:colour (vec3 1 0.5 0.5)
          #:scale (vec3 0.1 0.1 0.1)
          #:translate (vadd (value-now player-pos) (vec3-integral (vec3 0 0.01 0))))))
   fire-time-e 10))

(scene 
 (list
  (object 
   #:shape 'model
   #:filename "rocket.obj"
   #:rotate (vec3 -90 0 0)
   #:scale (vec3 0.2 0.2 0.2)
   #:translate player-pos
   #:colour (vec3 0 0 1))
  
  (map
   (lambda (pos)
     (let ((pos (vmul (vadd pos (vec3 -2.5 -2.5 -1)) 2)))
       (if (not (hold (when-e (collision-with-list? pos bullets 1)) false))
           (object 
            #:shape 'model
            #:filename "alien.obj"
            #:scale (vec3 0.3 0.3 0.3)
            #:colour (vec3 1 0 0)
            #:translate pos))))
   (make-vector-grid 4 4 1))
  
  bullets))

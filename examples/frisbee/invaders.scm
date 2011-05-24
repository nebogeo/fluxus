(require fluxus-018/frisbee)

(define (between t a b)
    (and (>= t a) (<= t b)))

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
   #:shape "rocket.obj"
   #:scale (vec3 0.2 0.2 0.2)
   #:translate player-pos
   #:colour (vec3 0 0 1))
  
  (map
   (lambda (pos)
     (let ((pos (vadd 
            (vmul (vadd pos (vec3 -2.5 -2.5 -1)) 2)
            (vadd pos (vec3-integral
                 (hold 
                    (map-e
                        (lambda (e)
                            (let ((tick (modulo (+ (inexact->exact (vec3-y pos)) (value-now seconds)) 10)))
                            (cond 
                                ((between tick 0 5)
                                    (vec3 0.001 0 0))
                                ((between tick 6 9) 
                                    (vec3 -0.001 0 0))
                                ((vec3 0 0 0)))))
                        fluxus-pulse) (vec3 0 0 0)))))))
       (if (not (hold (when-e (collision-with-list? pos bullets 0.2)) #f))
           (object 
            #:shape "alien.obj"
            #:scale (vec3 0.2 0.2 0.2)
            #:colour (vec3 1 0 0)
            #:translate pos))))
   (make-vector-grid 4 4 1))
  
  bullets))

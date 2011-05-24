; a stupid shoot-em-up example for frisbee
(require fluxus-018/frisbee)

(clear-colour (vector 0 0.4 1))

(define player-pos
  (vec3 (key-control-b #\d #\a 0.01)
        (+ (key-control-b #\w #\s 0.01) 5) 0))

(define fire-time-e (key-time-e #\x))

(define bullets 
  (factory
   (lambda (t)
     (if (< clock (+ t 1000))
         (object
          #:shape 'sphere 
          #:colour (vec3 1 0.5 0.5)
          #:scale (vec3 0.3 0.3 1)
          #:translate (vec3 (vec3-x (value-now player-pos))
                            (vec3-y (value-now player-pos))
                            (- (integral -0.05) 1)))))
   fire-time-e 50))
 
(define enemies
  (factory
   (lambda (t)
     (let loop ((c 10) (l '()))
        (let ((pos (vec3 (* (- c 5) 10) 0 (- (integral 0.1) 500))))
       (if (zero? c) l
            (loop (- c 1) 
                         (cons 
                       (if (not (hold (when-e (collision-with-list? pos bullets 1)) false))
                          (object
                           #:shape "alien.obj"
                           #:translate pos
                           #:colour (vec3 0.8 1 0.8))) 
                        l))))))
   (metro 2) 5))


(scene
 (list
  ; draw the player
  (object
   #:shape "rocket.obj"
   #:translate player-pos
   #:scale (vec3 0.4 0.4 0.4)
   #:rotate (vec3 0 180 45)
   #:colour (vec3 0.8 0.8 1))
      
  bullets   
  enemies
  
  ; draw the ground
  (object
   #:shape 'plane
   #:translate (vec3 0 -60 0)
   #:rotate (vec3 90 0 0)
   #:colour (vec3 0.0 0.5 0.0)
   #:scale (vec3 1000 1000 1000)
   #:hints (list 'unlit))
  ))

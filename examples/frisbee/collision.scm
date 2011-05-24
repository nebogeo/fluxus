(require fluxus-018/frisbee)

(define player-pos
  (vec3 (key-control-b #\d #\a 0.01)
        (+ (key-control-b #\w #\s 0.01) 5) 0))

(scene
 (list
  (object #:translate player-pos)
  (object 
   #:shape 'sphere
   #:translate (vec3 0 0 0)
   #:colour (collide-b 
             (lambda (a b) (rndvec)) 
             (vec3 1 1 1) (vec3 0 0 0) player-pos 2))))

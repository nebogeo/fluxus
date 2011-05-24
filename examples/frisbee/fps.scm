(require fluxus-018/frisbee)

(set-camera (mmul (mtranslate (vec3 0 0 -20)) (mrotate (vec3 45 0 0))))

(define (player)
  (letrec ([pos (vec3-integral dir)]               
           [dir (vtransform (vec3 (+ 0.000001 
                                     (key-press-b #\w 0.01 0)
                                     (key-press-b #\s -0.01 0)) 0 0)
                            (mrotate (vec3  0 (key-control-b #\a #\d 0.1) 0)))])
    (object 
     #:translate pos
     #:matrix (maim dir (vec3 0 1 0))
     #:colour (vec3 0 0 1)
     #:camera-lock #f)))

(define my-player (player))

(define (pickup pos)
  (object 
   #:translate (if (collide-b 
                    (lambda (a b) #t) 
                    #f 
                    pos 
                    (object-struct-translate my-player) 1)
                   (object-struct-translate my-player) pos)
   #:scale (vec3 0.3 3 0.3)
   #:colour (vec3 1 1 0)))
  
(scene 
 (list
  my-player
  (map
   (lambda (pos)
     (pickup (vmul (vsub pos (vec3 2.5 1 2.5)) 5)))
   (make-vector-grid 5 1 5))
  (object
   #:shape 'plane
   #:rotate (vec3 90 0 0)
   #:scale (vec3 50 50 50)
   #:colour (vec3 0.4 0.8 0.4))))


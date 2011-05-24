(require fluxus-018/frisbee)

(define player-pos
  (vec3 (key-control-b #\d #\a 0.01)
        (+ (key-control-b #\w #\s 0.01) 5) 0))

(scene
 (list
  (object
   #:translate player-pos
   #:scale (vec3 0.1 0.1 0.1))
  
  (factory
   (lambda (e)
     (define-values (follow-pos follow-dir)
       (letrec ([follow-pos (vec3-integral follow-dir)]               
                [follow-dir (vec3-integral 
                             (vmul (vnormalise 
                                    (vsub player-pos follow-pos)) 
                                   0.000001))])
         (values follow-pos follow-dir)))
     (object 
      #:shape 'cube
      #:colour (vec3 (flxrnd) (flxrnd) (flxrnd))
      #:scale (vec3 0.5 0.1 0.1)
      #:translate follow-pos
      #:matrix (maim follow-dir (vec3 1 0 0))))
   (metro 2) 10)))

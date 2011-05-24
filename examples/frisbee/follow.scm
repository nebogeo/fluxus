; frisbee version of the ball-on-string example
(require fluxus-018/frisbee)

(define player-pos
  (vec3 (key-control-b #\d #\a 0.01)
        (+ (key-control-b #\w #\s 0.01) 5) 0))

(define-values (follow-pos follow-dir)
 (letrec ([follow-pos (vec3-integral follow-dir)]
          [follow-dir (vmul (vsub player-pos follow-pos) 0.001)])
   (values follow-pos follow-dir)))

(scene
 (list
  (object 
   #:translate player-pos)
  (object
   #:colour (vec3 0.5 0.5 1)
   #:translate follow-pos
   #:matrix (maim (vnormalise follow-dir) (vec3 1 0 0)))
  (object 
   #:scale (vec3 0.1 0.1 0.1)
   #:translate (vadd follow-pos (vnormalise follow-dir)))))

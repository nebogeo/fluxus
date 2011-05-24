(require fluxus-018/frisbee)

(define player-pos
  (vec3 (key-control-b #\d #\a 0.01)
        (+ (key-control-b #\w #\s 0.01) 5) 0))

(scene
 (list
  
  (object #:translate player-pos)
  
  (factory
   (lambda (e)
     (define-values (follow-pos follow-dir)
       (letrec ([follow-pos (vec3-integral follow-dir)]
                [follow-dir (vmul (vsub player-pos follow-pos) (* 0.005 (flxrnd)))])
         (values follow-pos follow-dir)))
     (object
      #:colour (vec3 0.5 0.5 1)
      #:translate follow-pos
      #:matrix (maim follow-dir (vec3 1 0 0))))
   (metro 0.5) 5)))

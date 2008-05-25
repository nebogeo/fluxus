(require fluxus-015/frisbee)

(define player-pos
  (vec3 (key-control-b #\d #\a 0.01)
        (+ (key-control-b #\w #\s 0.01) 5) 0))

(define obs
  (collect-b 
   (metro 1) '() 
   (lambda (e lst)
     (let ((pos (vsub (vec3-integral (vec3 0 0.002 0)) (vec3 0 3 0))))
       (cons 
        (if (> (vdist pos player-pos) 2)
            (object 
             #:colour (vec3 1 0 0)
             #:translate pos))
      (truncate-list lst 4))))))

(scene 
 (list
  (object 
   #:translate player-pos
   #:colour (vec3 0 0 1))
  
  obs))

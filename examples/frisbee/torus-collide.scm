(require fluxus-018/frisbee)


(define player-pos
  (vec3 (key-control-b #\d #\a 0.01)
        (+ (key-control-b #\w #\s 0.01) 5) 0))

(define (collide-torus pos)
  (object
   #:shape 'torus
   #:translate pos
   #:scale (collide-b              
            (lambda (a b)
              (let ((sc (+ 0.6 (* 0.5 (cos (* 0.001 (value-now milliseconds)))))))
                (vec3 sc sc sc)))
            (vec3 1 1 1) pos player-pos 2)
 
   #:colour (collide-b
             (lambda (a b)
               (vec3 (+ 1 (* 0.5 (sin (* 0.001 (value-now milliseconds)))))
                     (+ 1 (* 0.5 (sin (* 0.0005 (value-now milliseconds)))))
                     (+ 1 (* 0.5 (sin (* 0.00034 (value-now milliseconds)))))))
             (vec3 1 1 1) pos player-pos 2)))

(scene
 (list
  (object
   #:translate player-pos)
  (map 
   (lambda (pos)
     (collide-torus (vadd (vmul (vsub pos (vector 6.5 6.5 1)) 1.5) 
                           (vector 0 1 0))))
   (make-vector-grid 10 10 1))))

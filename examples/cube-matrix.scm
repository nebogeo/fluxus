(define (draw-row x y z)
    (cond 
        ((not (zero? x))
            (translate (vector 1 0 0))
            (with-state  
                (let* ((vec (vtransform (vector 0 0 0) (get-transform)))
                   (dist (vdist vec (vector 5 (* 5 (+ 1 (sin (time)))) 5)))
                   (size (/ (max 0 (- 5 dist)) 5)))
                (scale (vector size size size)))
                (draw-cube))           
            (draw-row (- x 1) y z))))


(define (draw-flat-grid x y z)
    (cond 
        ((not (zero? y))
            (translate (vector 0 1 0))
            (with-state (draw-row x y z))           
            (draw-flat-grid x (- y 1) z))))

(define (draw-grid x y z)
    (cond 
        ((not (zero? z))
            (translate (vector 0 0 1))
            (with-state (draw-flat-grid x y z))
            (draw-grid x y (- z 1)))))

(every-frame (draw-grid 10 10 10)) 

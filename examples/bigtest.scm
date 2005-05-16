



(define (render n)
    (push)
    (scale (vmul (vector 1 1 1) (abs (sin (* (+ n (* (time) 10)) 0.1)))))
    (rotate (vector 0 (* n 10) 0))
    (translate (vector n 0 0))
    (draw-sphere)
    (pop)
    (if (< n 0)
        0
        (render (- n 1))))

(every-frame "(render 300)")    
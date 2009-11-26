(clear)
(blend-mode 'src-alpha 'one)
(hint-nozwrite)
(scale 1)
(texture (load-texture "splat.png"))
(define p (build-voxels 30 30 30))
(define t 0)

(every-frame (with-primitive p 
    (set! t (+ t 0.02))
    (for ((i (in-range 0 10)))
        (voxels-sphere-solid (vector (+ 0.5 (* 0.5 (sin (+ (* i 0.1) t)))) 0.5 0.5) 
            (if (odd? i) (vector 0 0 0) (vmul (vector 1 0.1 0.1) (* i 0.05)))
            (- 1 (/ i 10))))

    (voxels-box-solid (vector 0.5 0.5 -0.1) (vector 1.1 1.1 1.1) (vector 0 0 0))
    (voxels-calc-gradient)
    (voxels-point-light (vector 0 50 0) (vmul (vector 1 1 1) 0.05))))

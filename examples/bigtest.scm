
;8.8
(start-audio 2048 44100)
(clear)

(reset-camera)

(show-fps 1)

(define obs '())

(define (make n)
    (push)
    (opacity 0.5)
    (colour (vector 0 0 1))
    (scale (vmul (vector 1 1 1) (* 0.2 (+ n 1))))
    (set! obs (cons (build-nurbs-sphere 8 10) obs))
    (pop)
    (if (< n 0)
        0
        (make (- n 1))))

(define (animate l n)
    (grab (car l))
    (translate (vmul (vector (sin (+ n (time))) (sin (+ 1 (+ n (time)))) 0) 0.2))
    (ungrab)
    (if (eq? (cdr l) '())
        0
        (animate (cdr l) (+ n 1))))

(define (render n)
    (push)
    (texture (load-texture "textures/green.png"))
    (scale (vector 0.2 0.2 0.2))
    (scale (vadd (vector 1 1 1) 
        (vmul (vector 1 1 1) (abs (sin (* (+ n (* (time) 10)) 0.2))))))
    (rotate (vector 0 (* n 1) 0))
    (translate (vector (* n 0.1) 0 0))
    (draw-cube)
    (pop)
    (if (< n 0)
        0
        (render (- n 1))))

(make 10)

(define (frame)
    0
    (render 200)
    (animate obs 0)
    )

(every-frame "(frame)")    
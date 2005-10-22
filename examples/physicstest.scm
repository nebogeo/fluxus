
(define l '())

(define (sphere)
    (push)
    (colour (vector 1 1 1))    
    (scale (vector 0.5 0.5 1.5))
    (let ((ob (build-cube)))
        (set! l (cons ob l))    
        (active-box ob))
    (pop))

(define (line n)
    ;(rotate (vector 0 (* (flxrnd) 90) 0))
    (translate (vector (* 0.01 (flxrnd)) (+ 1 (* 0.01 (flxrnd))) (* 0.001 (flxrnd)) ))
    (sphere)
    (if (eq? 0 n)
        1
        (line (- n 1))))

(clear)
(collisions 1)
(ground-plane (vector 0 1 0) 0)
(show-axis 1)
(clear-colour (vector 0.5 0.1 0.7))
(line 20)
(desiredfps 500)
(gravity (vector 0 -0.1 0))

(define (render l)
    (grab (car l))
    (if (has-collided)
        (colour (vector (flxrnd) 0 0)))
    (ungrab)
    (if (eq? (cdr l) '())
        0
        (render (cdr l))))

(every-frame "(render l)")

    


(clear)
(collisions 1)
(desiredfps 1000)
(gravity (vector 0 -1 0))
(define list '())

(define (add t)
    (push)
    ;(opacity 0.4)
    (texture (load-texture "water.png"))
    (translate (vector 0 t 0))
    (scale (vector 0.5 0.8 0.5))
    (set! list (cons (build-cube) list))
    (active-box (car list))
    (set-mass (car list) 0.01)
    (pop))


(define (link l c)
    (if (eq? (cdr l) '())
        '()
        (begin
            (build-balljoint (car l) (car (cdr l)) (vector 0 (+ c 0.5) 0))
            (link (cdr l) (+ c 1)))))

(define (animate l c)
    (if (eq? l '())
        '()
        (begin
            (if (> (gh c) 0.5)
                (kick (car l) (vector 0 (* (gh c) 0.02) 0)))
            (animate (cdr l) (+ 1 c)))))

(ground-plane (vector 0 1 0) 0)

(define (array n)
    (add n)
    (if (eq? n 0)
        0
        (array (- n 1))))

(array 40)
(link list 0)
(desiredfps 1000)
;(lock-camera (cadddr list))
;(clear)
;(kick (car list) (vector 0 200 0))
;(blur 0.1)
(engine-callback "(animate list 0)")

;(twist (car list) (vector 1 0 0))

(clear)
(collisions 1)
(desiredfps 1000)
(gravity (vector 0 -1 0))
(define list '())

(define (add t)
    (push)
    (translate (vector 0 t 0))
    (scale (vector 0.7 1 0.2))
    (set! list (cons (build-cube) list))
    (active-box (car list))
    (pop))

(define (link l c)
    (build-balljoint (car l) (car (cdr l)) (vector 0 (+ c 0.5) 0))
    (if (eq? (cdr (cdr l)) '())
        '()
        (link (cdr l) (+ c 1))))
            
(define (array n)
    (add n)
    (if (eq? n 0)
        0
        (array (- n 1))))

(ground-plane (vector 0 1 0) 0)
(array 40)
(link list 0)


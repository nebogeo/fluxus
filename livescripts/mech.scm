
(define texnames '("mech/1.png" "mech/2.png" "mech/3.png" "mech/4.png" "mech/5.png"
    "mech/6.png" "mech/7.png" "mech/8.png" "mech/9.png" "mech/10.png"
    "mech/11.png" "mech/12.png" "mech/13.png" "mech/14.png" "mech/15.png"))


(define tr 8)
(define sc 1)
(define zpos 0)
(define t 0)
(define elements '())

(define (add-element tex)
    (push)
    (opacity 1) 
    (texture (load-texture tex))
    (translate (vector (* tr (flxrnd)) (* tr (flxrnd)) zpos))
    (set! zpos (+ 0.01 zpos))
    (set! t (* sc (flxrnd)))
    (scale (vector t t 1))
    (set! elements (cons (build-plane) elements))
    (pop))


(define (animate l c)
    (grab (car l))
    (rotate (vector 0 0 (* 10 (- (gh c) 0.5))))
    (ungrab)
    (if (eq? (cdr l) '())
        '()
        (animate (cdr l) (+ 1 c))))

(define (render)
    (animate elements 1))

(define (build-all l)
    (add-element (car l))
    (if (eq? '() (cdr l))
        '()
        (build-all (cdr l))))

(define (build n)
    (build-all texnames)
    (if (eq? n 0)
        0
        (build (- n 1))))

(clear)
(build  20)




(every-frame "(render)")


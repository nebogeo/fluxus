; an example of collisions, the ground plane and joints

(clear)
(collisions 1)
(define objs '())

; makes a new object for connection in the chain
(define (add t)
    (push)
    (translate (vector 0 t 0))
    (scale (vector 0.7 1 0.2))
    (set! objs (cons (build-cube) objs))
    (active-box (car objs))
    (pop))

; links all the objects in the list together
(define (link l c)
    (build-balljoint (car l) (car (cdr l)) (vector 0 (+ c 0.5) 0))
    (if (eq? (cdr (cdr l)) '())
        '()
        (link (cdr l) (+ c 1))))

; builds the objects          
(define (array n)
    (add n)
    (if (eq? n 0)
        0
        (array (- n 1))))

; set gravity up (I think this is default anyway, but try changing it)
(gravity (vector 0 -1 0))
; setup a groundplane to collide with
(ground-plane (vector 0 1 0) 0)
; build the objects
(array 40)
; join them together
(link objs 0)


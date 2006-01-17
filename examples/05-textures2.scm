; a script adapted from a live one I use

; some textures to use
; todo - get some new textures
(define texnames '("mech/1.png" "mech/2.png" "mech/3.png" "mech/4.png" "mech/5.png"
    "mech/6.png" "mech/7.png" "mech/8.png" "mech/9.png" "mech/10.png"
    "mech/11.png" "mech/12.png" "mech/13.png" "mech/14.png" "mech/15.png"))

; constants
(define tr 8)
(define sc 1)
(define zpos 0)
(define t 0)

; a list to hold the objects
(define elements '())

; make a new textured plane 
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

; build a plane for every texture in the list, and add it to
; the elements list
(define (build-all l)
    (add-element (car l))
    (if (eq? '() (cdr l))
        '()
        (build-all (cdr l))))

; call build-all n times
(define (build n)
    (build-all texnames)
    (if (eq? n 0)
        0
        (build (- n 1))))

; rotate the objects in the list according to the audio
(define (animate l c)
    (grab (car l))
    (rotate (vector 0 0 (* 10 (- (gh c) 0.5))))
    (ungrab)
    (if (eq? (cdr l) '())
        '()
        (animate (cdr l) (+ 1 c))))

(define (render)
    (animate elements 1))

(clear)
(build  20)
(every-frame "(render)")

; simple script to render multiple textured planes on top of each other

; this script also demonstrates a restriction of alpha compositing in opengl, 
; in that the order the objects get rendered in effects the visual result. 
; to acheive the best results, render from back to front.

; a list to hold our objects
(define objs '())

; build and store a textured plane
(define (ob)
    (push)
    (opacity 0.05)
    (colour (vector 1 1 1))
    (texture (load-texture "textures/green.png"))
; uncomment the line below to play with some of the blendmodes
;    (blend-mode "one-minus-src-alpha" "src-alpha")
    (set! objs (cons (build-plane) objs))
    (apply (car objs))
    (pop))


; build multiple textured planes
(define (line n)
    (translate (vector 0 0 -0.01))
    (scale (vector 1.1 1.1 1.1))
    (ob)
    (if (eq? 0 n)
        1
        (line (- n 1))))

; loop through the list, rotating the objects
(define (update l c s)
    (if (null? l)
        '()
        (begin 
            (grab (car l))
            (rotate (vector 0 0 (* (- 0.5 (sin (* s 0.1)))1)))
            (ungrab)
            (update (cdr l) (+ c 1) (+ s 1)))))

(show-axis 0)
(clear)
(line 10)
(blur 0.1)
(every-frame "(update objs 1 1)")

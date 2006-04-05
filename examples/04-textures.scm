; simple script to render multiple textured planes on top of each other

; this script also demonstrates a restriction of alpha compositing in opengl, 
; in that the order the objects get rendered in effects the visual result. 
; to acheive the best results, render from back to front.(turn the camera to
; the other side of the objects to see what I'm going on about)

; a list to hold our objects
(define objs '())

; build and store a textured plane
(define (ob)
    (push)
    (opacity 1)
    (colour (vector (flxrnd) (flxrnd) (flxrnd)))
    (texture (load-texture "textures/transp.png"))
; uncomment the line below to play with some of the blendmodes
    ;(blend-mode "src-alpha" "src-color")
    (set! objs (cons (build-plane) objs))
    (apply-transform (car objs))
    (pop))


; build multiple textured planes
(define (line n)
    (translate (vector 0 0 0.01))
    (scale (vector 1.2 1.2 1.2))
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
            (rotate (vector 0 0 (sin (+ (time) (* s 0.9)))))
            (ungrab)
            (update (cdr l) (+ c 1) (+ s 1)))))

(clear)
(line 10)
(blur 0.1)
(every-frame (update objs 1 1))

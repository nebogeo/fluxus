; an example of more complex turtle modelling

(define pi 3.141)

; builds a circular strip of triangles
(define (section npoints radius-s radius-e turn width)
    (define (circ npoints angle distance-s distance-e)
        (turtle-turn (vector 0 angle 0))
        (turtle-push)
        (turtle-push)
        (turtle-move radius-s)
        (turtle-turn (vector 0 90 0))
        (turtle-move distance-s)
        (turtle-vert)
        (turtle-pop)
        
        (turtle-turn (vector 0 0 90))
        (turtle-turn turn)
        (turtle-move width)
        (turtle-turn (vector 0 0 -90))

        (turtle-push)
        (turtle-move radius-e)
        (turtle-turn (vector 0 90 0))
        (turtle-move distance-e)
        (turtle-vert)
        (turtle-pop)
        (turtle-pop)
        (if (zero? npoints)
            0
            (circ (- npoints 1) angle distance-s distance-e)))
    (turtle-push)
    (circ npoints (/ 360 npoints)
        (/ (* (* pi radius-s) (* pi radius-s)) npoints)
        (/ (* (* pi radius-e) (* pi radius-e)) npoints))
    (turtle-pop))

; builds a stack of strip sections, using the function specified to change
; the width of the tube, and turning it per slice
(define (stack npoints radius width slices widthfn turnfn)
    (define (_stack slice)
        (section npoints (widthfn (/ slice slices))
            (widthfn (/ (- slice 1) slices)) (turnfn (/ slice slices)) width)
        (turtle-turn (vector 0 0 90))
        (turtle-turn (turnfn (/ slice slices)))
        (turtle-move width)
        (turtle-turn (vector 0 0 -90))
        (if (zero? slice)
            0
            (_stack (- slice 1))))
    (_stack slices))


(clear)
(specular (vector 1 1 1))
(shinyness 10)
(colour (vector 0 0 1))
(wire-colour (vector 1 1 1))

(define obj 0)

(define (wfn n)
    (* 0.7 (* (- 1 n) 0.6) (+ 3.1 (sin (* n (sin (* 0.3 (time))) 100)))))

(define (tfn n)
    (vector (* n 30 (sin (* 0.1 (time)))) 0 0))

(define (render)
    (destroy obj)
    (turtle-reset)
    (turtle-prim 0)
    (stack 12 0.4 0.8 50 wfn tfn)    
    (set! obj (turtle-build))
    (grab obj)
    (recalc-normals 1)
    (ungrab))

(every-frame (render))

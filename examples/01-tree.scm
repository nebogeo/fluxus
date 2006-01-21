; a recursivly branching tree

(clear)
(show-axis 1)

(define (tree n)
    (push)
    (colour (vector 0.7 0.8 0.5))
    (rotate (vector 0 30 0))
    (translate (vector 0 0.6 0))
    (scale (vector 0.8 0.8 0.8))
    
    (push)
    (scale (vector 0.2 1 0.2))
    ; draw a branch 
    (draw-cube)
    (pop)

    (if (eq? 0 n)
        1
        (begin
            ; draw the 'left' brach... 
            (rotate (vector 0 0 45))
            (translate (vector 0.3 0.15 0))
            (tree (- n 1))

            ; then draw the 'right' branch...
            (rotate (vector 0 0 -90))
            (translate (vector -0.15 -0.15 0))
            (tree (- n 1))))
    (pop))

(define (loop) (tree 10))

(every-frame "(loop)")


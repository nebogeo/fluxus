; a recursivly branching tree

(define (tree n)
    (cond ((not (zero? n))
        (with-state
            (colour (vector 0.7 0.8 0.5))
            (rotate (vector 0 30 0))
            (translate (vector 0 0.6 0))
            (scale (vector 0.8 0.8 0.8))
    
            ; draw a branch 
            (with-state
                (scale (vector 0.2 1 0.2))
                (draw-cube))

            ; draw the 'left' subtree... 
            (with-state
                (translate (vector 0.1 0.3 0))
                (rotate (vector 0 0 45))
                (tree (- n 1)))

            ; then draw the 'right' subtree...
            (with-state
                (translate (vector -0.1 0.3 0))
                (rotate (vector 0 0 -45))
                (tree (- n 1)))))))

(clear)
(show-axis 1)
(every-frame (tree 10))


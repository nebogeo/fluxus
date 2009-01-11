; build a random heirachical structure
(define (build-heir depth)
    (with-state
        (let ((p (with-state
                        (translate (vector 2 0 0))
                        (scale 0.9)
                        (build-cube))))
            (when (> depth 0)
                (parent p)            
                (for ((i (in-range 0 5)))
                    (when (zero? (random 3))
                        (rotate (vector 0 0 (* 45 (crndf))))
                        (build-heir (- depth 1))))))))

; navigate the scene graph and print it out
(define (print-heir children)
    (for-each
        (lambda (child)
            (with-primitive child
                (printf "id: ~a parent: ~a children: ~a~n" child (get-parent) (get-children))
                (print-heir (get-children))))
        children))

(clear)
(build-heir 5)
(print-heir (get-children))

(define col 5)
(gain 10)

(define (draw n)
    (rotate (vector 0 (* 100 (gh n)) 0))
    (push)
    (opacity 0.5)
    (translate (vector n (* 10 (gh n)) 0))
    (colour (vector (* col (gh (+ n 1))) 
                    (* col (gh n) )
                    (* col (gh (- n 5)))))
    (scale (vector 1(* 5 (gh n) )1))
    (draw-cube)
    (pop)
    (if (eq? n 0)
        1
        (begin 
        (draw (- n 1))
        (rotate (vector 45 0 0))
        (draw (- n 1)))))

(define (render)
    (draw 5))
(clear)
(every-frame "(render)")


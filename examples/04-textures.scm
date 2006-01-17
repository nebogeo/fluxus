(define list '())

(define (ob)
    (push)
    (opacity 0.05)
    (colour (vector 1 1 1))
    (texture (load-texture "textures/test.png"))
;    (blend-mode "one-minus-src-alpha" "src-alpha")
    (set! list (cons (build-plane) list))
    (apply (car list))
    ;parent (car list))
    (pop))

(define (line n)
    (translate (vector 0 0 -0.01))
    (scale (vector 1.1 1.1 1.1))
    (ob)
    (if (eq? 0 n)
        1
        (line (- n 1))))


(define (update l c s)
    (if (null? l)
        '()
        (begin 
            (grab (car l))
            (rotate (vector 0 0 (*(- 0.5 (sin (* s 0.1)))1)))
            ;(scale (vector (gh s) (gh s) (gh s) ))
            (ungrab)
            (update (cdr l) (+ c 1) (+ s 1)))))

(show-axis 0)
(clear)
(line 10)
(blur 0.1)
(every-frame "(update list 1 1)")

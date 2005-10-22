
(clear)

(define f (load-texture "textures/font2.png"))

(define (build n)
    (translate (vector 1.1 0 0))
    (let ((id (build-cube)))
        (push)
        (translate (vector 0 1 0))
        (scale (vector 0.2 0.2 0.2))
        (texture f)
        (build-text (number->string id))
        (pop))
    (if (< n 0)
        0
        (build (- n 1))))

(define (render)
    (if (mouse-button 1)
        (begin
        (grab (mouse-over))
        (rotate (vector 1 0 0))
        (colour (vector (flxrnd)(flxrnd)(flxrnd)))
        (ungrab))))


(build 10)
(every-frame "(render)")
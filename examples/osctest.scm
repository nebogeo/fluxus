(define out 0)
(clear-colour (vector 1 1 1))
(texture (load-texture "textures/scribblefont.png"))

(define (osctest)
    (destroy out)
    (set! out (build-text (peek-osc))))

(start-osc "88001")

(every-frame "(osctest)")

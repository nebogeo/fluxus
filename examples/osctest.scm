(define out 0)
(clear-colour (vector 1 1 1))
(texture (load-texture "scribblefont.png"))

(define (osctest)
    (destroy out)
    (set! out (build-text (peek-osc)))
    (display (from-osc "/float" 0)) (newline)
    (display (from-osc "/astring" 0)) (newline)
    (display (from-osc "/test" 0)) (newline)
    (display (from-osc "/test" 1)) (newline))

(start-osc "88000")

(engine-callback "(osctest)")

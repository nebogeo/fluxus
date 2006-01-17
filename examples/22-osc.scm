(define out 0)
(hint-unlit)
(texture (load-texture "textures/scribblefont.png"))

(define (osctest)
    (destroy out)
    (set! out (build-text (osc-peek))))

(osc-source "88001")

(every-frame "(osctest)")

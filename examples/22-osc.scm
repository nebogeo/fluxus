(define out (build-cube))
(hint-unlit)
(texture (load-texture "textures/.png"))

(define (osctest)
    (destroy out)
    (set! out (build-text (osc-peek))))

(osc-source "4444")

(every-frame (osctest))

(clear)

(scale (vector 20 15 1))

(define p (build-pixels 512 512 #t 2))

(define frame 0)

(every-frame
    (let ([tex (pixels->texture p frame)]
          [ptex (pixels->texture p (bitwise-xor frame 1))])
        (with-primitive p
            (pixels-render-to tex)
            (pixels-display tex))
        (with-pixels-renderer p
            (clear-colour 1)
            (with-state
                (texture ptex)
                (rotate (vector (* 60 (time)) 45 0))
                (scale 6)
                (draw-cube)))
        (set! frame (bitwise-xor frame 1))))


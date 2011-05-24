(require fluxus-018/tricks)
(clear)
(define p (with-state
    (hint-normal)
    (load-primitive "bot.obj")))
(define q (with-state
    (translate (vector 2 0 0))
    (load-primitive "widget.obj")))
(define r (with-state
    (translate (vector 4 0 0))
    (build-sphere 10 10)))

(with-primitive p
    ;(poly-convert-to-indexed)
    (recalc-normals 1) 
    )

(cheap-toon p 0.1 (vector 1 1 1))
(cheap-toon q 0.1 (vector 1 1 1))
(cheap-toon r 0.1 (vector 1 1 1)) 

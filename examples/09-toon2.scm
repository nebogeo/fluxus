(save-name "toonanim.scm")
(clear)
(define dirlight1 (vtransform (vector 0 1 0) (mrotate (vector 45 45 0))))

(texture (load-texture "textures/gradient.png"))
;(show-axis 1)
(hint-unlit)
;(hint-wire)
;(line-width 2)

(define (toon-light n)
    (let ((lighting (vdot (pdata-get "n" n) dirlight1)))
        (if (< lighting 0) (set! lighting 0.1)) 
        (if (> lighting 0.95) (set! lighting 0.95)) 
        (pdata-set "t" n (vector lighting 0 0)))
    (if (< n 1)
        0
        (toon-light (- n 1))))

(define (deform n)
    (pdata-set "p" n (vadd (pdata-get "pref" n) 
        (vmul (pdata-get "nref" n)
        (* (sin (* (+ (vector-ref (pdata-get "p" n) 1) (* (time) 0.3)) 9)) 0.2))))
    (if (< n 1)
        0
        (deform (- n 1))))


(colour (vector 0.9 0.5 1))
(define s (build-sphere 20 20))
(grab s)
(pdata-copy "p" "pref")
(pdata-copy "n" "nref")
(ungrab)

(define (render)
    (grab s)
    (pdata-copy "pref" "p")
    (deform (pdata-size))
    (recalc-normals)
    (toon-light (pdata-size))
    (ungrab))

(every-frame "(render)") 
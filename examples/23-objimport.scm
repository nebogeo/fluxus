(clear)
(load "../scm/objimport.scm")

(define dirlight1 (vtransform (vector 0 1 0) (mrotate (vector 45 45 0))))
(texture (load-texture "textures/gradient.png"))
(hint-unlit)

(define (toon-light n)
    (let ((lighting (vdot (pdata-get "n" n) dirlight1)))
        (if (< lighting 0) (set! lighting 0.1))     ; reverse facing polys are nearly black
        (pdata-set "t" n (vector lighting 0 0)))
    (if (< n 1)
        0
        (toon-light (- n 1))))

(scale (vector 20 20 20))
(let ((obj (obj-make (obj-load "meshes/bunny-1500.obj"))))
(grab obj)
(toon-light (pdata-size))
(ungrab))

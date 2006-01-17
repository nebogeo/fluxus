; a fancier example using textured planes - another example of the 
; problems with alpha blending and drawing order unfortunately

(define col (vector (flxrnd) (flxrnd) (flxrnd)))
(clear-colour (vector 0 0.5 0.5))

(define (init n)
    (pdata-set "p" n (vector (flxrnd)(flxrnd) (* n 0.01)))
    (pdata-set "c" n col)
    (let ((s (flxrnd)))
        (pdata-set "s" n (vmul (vector 1 1 1) s)))
    (pdata-set "vel" n (vmul (vector (- (flxrnd) 0.5) (flxrnd) 0) 0.01))
    (if (< n 1)
        0
        (init (- n 1))))

(define (move)
    (grab p)
    (pdata-op "+" "p" "vel")
    (ungrab))


(clear)
(texture (load-texture "textures/smoke.png"))
(define p (build-particles 200))
(grab p)
(pdata-add "vel" "v")
(init (pdata-size))
(ungrab)
(show-axis 1)

(every-frame "(move)")

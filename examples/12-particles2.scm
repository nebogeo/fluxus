; a fancier example using textured sprites

(define col (vector (flxrnd) (flxrnd) (flxrnd)))
(clear-colour (vector 0 0.5 0.5))

(define (init n)
    (pdata-set "p" n (vector (flxrnd)(flxrnd)(flxrnd)))
    (pdata-set "c" n col)
    (let ((s (flxrnd)))
        (pdata-set "s" n (vmul (vector 1 1 1) s)))
    (pdata-set "vel" n (vmul (vector (- (flxrnd) 0.5) (flxrnd) (- (flxrnd) 0.5)) 0.01))
    (if (< n 1)
        0
        (init (- n 1))))

(define (move)
    (grab p)
    (pdata-op "+" "p" "vel")
    (ungrab))


(clear)

(hint-ignore-depth)
(blend-mode 'src-alpha 'one)
(texture (load-texture "smoke.png"))
(define p (build-particles 200))
(grab p)
(pdata-add "vel" "v")
(init (pdata-size))
(ungrab)

(every-frame (move))

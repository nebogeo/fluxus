; a fancy particles example using textured sprites
(clear)

(define col (vector (flxrnd) (flxrnd) (flxrnd)))
(clear-colour (vector 0 0.5 0.5))

(define particles (with-state
    (hint-ignore-depth)
    (blend-mode 'src-alpha 'one)
    (texture (load-texture "smoke.png"))
    (build-particles 200)))

(with-primitive particles
    (pdata-add "vel" "v")
    ; init everything
    (pdata-map! (lambda (p) (vector (flxrnd) (flxrnd) (flxrnd))) "p")
    (pdata-map! (lambda (c) col) "c")
    (pdata-map! 
        (lambda (s) 
            (let ((v (flxrnd)))
                (vector v v v)))
        "s")
    (pdata-map! 
        (lambda (vel) 
            (vmul (vector (- (flxrnd) 0.5) 
                          (flxrnd) 
                          (- (flxrnd) 0.5)) 0.01))
        "vel"))

; pfuncs are an attempt to speed up animation loops, by
; running changes over all pdata elements in one scheme command
(define add-vel-to-p (make-pfunc 'arithmetic))

; first it needs setting up
(pfunc-set! add-vel-to-p '(operator "add" 
                           src "vel" 
                           other "p" 
                           dst "p"))

(define (animate)
    (with-primitive particles
        ; then we just need to run it
        (pfunc-run add-vel-to-p)))

(every-frame (animate))

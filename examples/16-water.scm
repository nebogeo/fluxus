(clear)
(define w 20)
(define h 20)
(define clamp 0.004)
(define trans 0.06)

(define (init n)
    (pdata-set "p" n (vadd (pdata-get "p" n) (vector 0 (* (- (flxrnd) 0.5) 0.1) 0)))
    (if (< n 0)
        0
        (init (- n 1))))

(define (calc n)
    (let ((result (vector 0 0 0)) (p (vector-ref (pdata-get "p" n) 1)))
        (if (> p 0)
            (set! result (vector 0 -0.001 0))
            (set! result (vector 0 0.001 0)))

        (set! result (vadd result (vmul (pdata-get "vel" (- n w)) trans))) 
        (set! result (vadd result (vmul (pdata-get "vel" (+ n w)) trans))) 
        (set! result (vadd result (vmul (pdata-get "vel" (- n 1)) trans))) 
        (set! result (vadd result (vmul (pdata-get "vel" (+ n 1)) trans)))
        
        (pdata-set "vel" n (vadd (pdata-get "vel" n) result)))

        (if (< (vector-ref (pdata-get "vel" n) 1) (- clamp))
            (pdata-set "vel" n (vector 0 (- clamp) 0))) 
        (if (> (vector-ref (pdata-get "vel" n) 1) clamp)
            (pdata-set "vel" n (vector 0 clamp 0))) 

    (if (< n 0)
        0
        (calc (- n 1))))

(define (render)
    (grab s)
    (if  (key-pressed " ")
        (let ((pos (* (flxrnd) (pdata-size))))
            (pdata-set "p" pos (vadd (pdata-get "p" pos) 
                (vector 0 (* (- (flxrnd) 0.5) 0.5) 0)))))
    (calc (pdata-size)) 
    (pdata-op "+" "p" "vel")
    ;(pdata-op "*" "vel" 0.99)
    (recalc-normals)
    (ungrab))

(push)
(scale (vector 10 30 10))
(texture (load-texture "textures/yellow.png"))
(define s (build-nurbs-plane w h))
(pop)

(grab s)
(pdata-add "vel" "v")
(init (pdata-size))
(ungrab)

(every-frame "(render)")

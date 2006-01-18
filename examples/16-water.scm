; a simple, and really quite wrong script that looks like it could be made into a 
; water simulation of some kind, but shows a technique which works on the geometry 
; directly, and treats the vertex data as a simple cellular automata

(clear)

; dimensions of the grid 
(define w 20)
(define h 20)

; simulation constants 
(define clamp 0.004) ; stops the simulation going out of control
(define trans 0.06)  ; the amount the energy is transmitted to the neighboring vertex

; start off the simulation with random point heights (this simulation only works in Y)
(define (init n)
    (pdata-set "p" n (vadd (pdata-get "p" n) (vector 0 (* (- (flxrnd) 0.5) 0.1) 0)))
    (if (< n 0)
        0
        (init (- n 1))))

; do the simulation
(define (calc n)
    (let ((result (vector 0 0 0)) (p (vector-ref (pdata-get "p" n) 1))) 
        (if (> p 0)
            (set! result (vector 0 -0.001 0)) ; we are above "sea level" head down
            (set! result (vector 0 0.001 0))) ; we are below "sea level" head up

		; mix in the surrounding verts to transmit energy around
        (set! result (vadd result (vmul (pdata-get "vel" (- n w)) trans))) 
        (set! result (vadd result (vmul (pdata-get "vel" (+ n w)) trans))) 
        (set! result (vadd result (vmul (pdata-get "vel" (- n 1)) trans))) 
        (set! result (vadd result (vmul (pdata-get "vel" (+ n 1)) trans)))
        
		; add the result to the existing velocity
        (pdata-set "vel" n (vadd (pdata-get "vel" n) result)))

		; clamp the velocity - this stops the simulation going too fast and blowing up
        (if (< (vector-ref (pdata-get "vel" n) 1) (- clamp))
            (pdata-set "vel" n (vector 0 (- clamp) 0))) 
        (if (> (vector-ref (pdata-get "vel" n) 1) clamp)
            (pdata-set "vel" n (vector 0 clamp 0))) 

    (if (< n 0)
        0
        (calc (- n 1))))

(define (render)
    (grab s)
    (calc (pdata-size)) 
    (pdata-op "+" "p" "vel")
    (recalc-normals)
    (ungrab))

(push)
(scale (vector 10 30 10))
(define s (build-nurbs-plane w h))
(pop)

(grab s)
(pdata-add "vel" "v")
(init (pdata-size))
(ungrab)

(every-frame "(render)")

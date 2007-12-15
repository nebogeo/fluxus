; a simple script that looks like it could be made into a 
; water simulation of some kind

(clear)

; dimensions of the grid 
(define w 20)
(define h 20)

; simulation constants 
(define clamp 0.004) ; stops the simulation going out of control
(define trans 0.06)  ; the amount the energy transmitted to 
                     ; the neighboring vertex

(define (simulate n)
    (cond ((not (zero? n))
        (let ((result (vector 0 0 0)) 
              (p (vector-ref (pdata-get "p" n) 1))) 
    
            (if (> p 0)
                ; we are above "sea level" head down
                (set! result (vector 0 -0.001 0)) 
                ; we are below "sea level" head up
                (set! result (vector 0 0.001 0))) 
    
            ; mix in the surrounding verts to transmit energy around
            (set! result (vadd result 
                (vmul (pdata-get "vel" (- n w)) trans))) 
            (set! result (vadd result 
                (vmul (pdata-get "vel" (+ n w)) trans))) 
            (set! result (vadd result 
                (vmul (pdata-get "vel" (- n 1)) trans))) 
            (set! result (vadd result 
                (vmul (pdata-get "vel" (+ n 1)) trans)))
            
            ; add the result to the existing velocity
            (pdata-set "vel" n (vadd (pdata-get "vel" n) result)))
    
            ; clamp the velocity - this stops the 
            ; simulation going too fast and blowing up
            (if (< (vector-ref (pdata-get "vel" n) 1) (- clamp))
                (pdata-set "vel" n (vector 0 (- clamp) 0))) 
            (if (> (vector-ref (pdata-get "vel" n) 1) clamp)
                (pdata-set "vel" n (vector 0 clamp 0))) 
            (simulate (- n 1)))))

(define (render)
    (with-primitive s
        (simulate (pdata-size)) 
        (pdata-op "+" "p" "vel")
        (recalc-normals 1)))

(define s (with-state
    (specular (vector 1 1 1))
    (shinyness 3)
    (texture (load-texture "test.png"))
    (scale (vector 10 30 10))
    (build-nurbs-plane w h)))

(with-primitive s
    (pdata-add "vel" "v")
    ; start off the simulation with random point heights 
    ; (the simulation only works in Y)
    (pdata-map! 
        (lambda (p)
            (vadd p (vector 0 (* (- (flxrnd) 0.5) 0.1) 0)))
        "p"))

(every-frame (render))

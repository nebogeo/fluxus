; an example of passing parameters to a glsl shader, where all the
; deformation and colouring is done on the GPU.

; set the testcol pdata with a random colour for every vertex
(define (set-cols n)
    (pdata-set "testcol" n (vector (flxrnd) (flxrnd) (flxrnd)))
    (if (zero? n)
        0
        (set-cols (- n 1))))

(clear)
(fluxus-init)
(push)
; assign the shaders to the surface
(shader "simplevert.glsl" "simplefrag.glsl")
(define s (build-sphere 20 20))
(pop)

(grab s)
; add and set the pdata - this is then picked up in the vertex shader 
; as an input attribute called "testcol"
(pdata-add "testcol" "v")
(set-cols (pdata-size))
(ungrab)

(define (animate)
    (grab s)
    ; animate the deformamount uniform input parameter 
    (shader-set! (list "deformamount" (cos (time))))
    (ungrab))

(every-frame (animate))


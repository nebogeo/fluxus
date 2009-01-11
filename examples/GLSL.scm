; an example of passing parameters to a glsl shader, where all the
; deformation and colouring is done on the GPU. if you get a normal
; looking sphere, then you need an OpenGL2 support on your driver 
; and gfx card - but don't worry, there is lots you can do without
; shaders!

; you need to have built fluxus with GLSL=1
(clear)

;(fluxus-init) ; this is important to add when using shaders 
              ; at the moment, it will be moved somewhere
              ; to run automatically...

(define s (with-state
    ; assign the shaders to the surface
    (shader "simple.vert.glsl" "simple.frag.glsl")
    (build-sphere 20 20)))
    
(with-primitive s
    ; add and set the pdata - this is then picked up in the vertex shader 
    ; as an input attribute called "testcol"
    (pdata-add "testcol" "v")
    ; set the testcol pdata with a random colour for every vertex
    (pdata-map! 
        (lambda (c) 
            (rndvec)) 
        "testcol"))
    
(define (animate)
    (with-primitive s
        ; animate the deformamount uniform input parameter 
        (shader-set! (list "deformamount" (cos (time))))))

(every-frame (animate))


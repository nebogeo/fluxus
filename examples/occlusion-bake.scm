; example baking an ambient occlusion texture map by
; doing lots of raytracing - watch the texture 
; slowly build up

; as raytracing is really slow and painful to do, it's
; a good idea to do it once and store the results in a 
; texture for use later on. the downside is that it won't 
; update if the model deforms or moves.

; need to include the optional extras
(require fluxus-018/tricks)

(clear)

; make the texture map
(define tex (with-state 
             (translate (vector 1 0 0))
             (build-pixels 256 256)))

; load in a complex model to test
(define s (with-state
           (rotate (vector 90 0 0))
           ; apply the pixel prim as texture
           (texture (pixels->texture tex)) 
           (load-primitive "octopus.obj")))

(define (run)
  (with-primitive tex
                  (pixels-clear (vector 1 1 1 1))
                  (occlusion-texture-bake tex s 30 10 2 #f)

                  ; uncomment to save the texture out - give
                  ; it some gaussian blur and contrast in the gimp,
                  ; and apply it to the model as normal with (hint-unlit)
                  ;(save-primitive "octopus.occ.png")
                  ))

; call the function in a thread, so fluxus stays active,
; and you can watch the raytracing happen
(thread run)


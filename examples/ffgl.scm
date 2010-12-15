
(clear)

; pixelprimitive with 2 textures and an active renderer
(define p (build-pixels 256 256 #t 2))

; load plugin FFGLTile, available from the FreeFrame SDK
; at http://www.freeframe.org
(define plugin (ffgl-load "FFGLTile" 256 256))

(with-ffgl plugin
    (for ([i (ffgl-get-info)]) ; print plugin information
        (printf "~a~n" i))
    (printf "~a~n" (ffgl-get-parameters)) ; parameter names as strings
    (ffgl-process p ; output pixel primitive
        (pixels->texture p 1) ; output texture
        (pixels->texture p 0))) ; input texture

(with-primitive p
    ; the renderer of the pixelprimitive renders to texture 0
    (pixels-render-to (pixels->texture p 0))
    ; the pixel primitive is displayed using texture 1
    (pixels-display (pixels->texture p 1)))
(define (anim)
    ; set plugin parameters as keywords arguments
    (with-ffgl plugin
        (ffgl-set-parameter! #:tilex (/ (mouse-x) (vx (get-screen-size)))
            #:tiley (/ (mouse-y) (vy (get-screen-size)))))
    ; render to the input pixelprimitive
    (with-pixels-renderer p
        (with-state
            (clear-colour #(0 1 0))
            (scale 5)
            (rotate (vector (* 50 (time)) -17 (* -35 (time))))
            (draw-cube))))

(every-frame (anim))

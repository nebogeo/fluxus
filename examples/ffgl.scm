; FreeFrame GL example
; for more information on FreeFrame plugins please visit
; http://www.freeframe.org

(clear)

(define p (build-pixels 256 256 #t)) ; input pixelprimitive

(translate (vector 1.1 0 0))

; output pixelprimitive - rendering is not active
; otherwise it would overwrite the plugin output
(define op (build-pixels 256 256))

; load plugin FFGLTile, available from the FreeFrame SDK
; at http://www.freeframe.org
(define plugin (ffgl-load "FFGLTile" 256 256))

(with-ffgl plugin
    (for ([i (ffgl-get-info)]) ; print plugin information
        (printf "~a~n" i))
    (printf "~a~n" (ffgl-get-parameters)) ; parameter names as strings
    (ffgl-process op p)) ; set destination and source pixelprimitives

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


;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; ffgl
;; FreeFrame is a cross platform real-time video effects plugin system.
;; Fluxus supports FreeFrame 1.5 also known as FreeFrameGL or FFGL. FF CPU
;; software rendering plugins are not supported at the moment.
;; For more information visit http://www.freeframe.org
;; Example:
;; (clear)
;;
;; ; pixelprimitive with 2 textures and an active renderer
;; (define p (build-pixels 256 256 #t 2))
;;
;; ; load the FFGLTile plugin from the FreeFrame SDK
;; (define plugin (ffgl-load "FFGLTile" 256 256))
;;
;; (with-ffgl plugin
;;   (for ([i (ffgl-get-info)]) ; print plugin information
;;        (printf "~a~n" i))
;;   (printf "~a~n" (ffgl-get-parameters)) ; parameter names as strings
;;   (ffgl-process p ; pixel primitive
;;                (pixels->texture p 1) ; output texture
;;                (pixels->texture p 0))) ; input texture
;;
;; (with-primitive p
;;    ; the renderer of the pixelprimitive renders to texture 0
;;    (pixels-render-to (pixels->texture p 0))
;;    ; the pixel primitive is displayed using texture 1
;;    (pixels-display (pixels->texture p 1)))
;; (define (anim)
;;    ; set plugin parameters as keywords arguments
;;    (with-ffgl plugin
;;        (ffgl-set-parameter! #:tilex (/ (mouse-x) (vx (get-screen-size)))
;;                             #:tiley (/ (mouse-y) (vy (get-screen-size)))))
;;    ; render to the input pixelprimitive
;;    (with-pixels-renderer p
;;        (with-state
;;            (clear-colour #(0 1 0))
;;            (scale 5)
;;            (rotate (vector (* 50 (time)) -17 (* -35 (time))))
;;            (draw-cube))))
;;
;; (every-frame (anim))
;; EndSectionDoc

#lang racket/base
(require "fluxus-modules.ss")
(require "tasks.ss")
(provide 
	with-ffgl
	ffgl-set-parameter!)
 
;; StartFunctionDoc-en
;; with-ffgl ffgl-pluginid expression ...
;; Returns: result of last expression
;; Description:
;; Allows you to work with the specified FFGL plugin.
;; Example:
;; (clear)
;; (define plugin (ffgl-load "FFGLTile" 256 256))
;;
;; (with-ffgl plugin
;;   (for ([i (ffgl-get-info)])
;;        (printf "~a~n" i)))
;; EndFunctionDoc

(define-syntax with-ffgl
  (syntax-rules ()
    ((_ a b ...)
     (begin
       (ffgl-push a)
       (let ((r (begin b ...)))
         (ffgl-pop)
         r)))))

;; StartFunctionDoc-en
;; ffgl-set-parameter! parameter-name-keyword parameter-value ...
;; Returns: void
;; Description:
;; Sets ffgl plugin parameters.
;; Example:
;; (clear)
;; (define plugin (ffgl-load "FFGLTile" 256 256))
;;
;; (with-ffgl plugin
;;        (ffgl-set-parameter! #:tilex .5 #:tiley .2))
;;
;; EndFunctionDoc

(define ffgl-set-parameter!
  (make-keyword-procedure
	(lambda (kws kw-args)
		(ffgl-set-parameter-list
			(apply append
				(for/list ([kw kws]
						   [arg kw-args])
					(list (string->symbol (keyword->string kw)) arg)))))))


;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; ffgl
;; FreeFrame is a cross platform real-time video effects plugin system.
;; Fluxus supports FreeFrame 1.5 also known as FreeFrameGL or FFGL. FF CPU
;; software rendering plugins are not supported at the moment.
;; For more information visit http:;;www.freeframe.org
;; Example:
;; (clear)
;;
;; (define p (build-pixels 256 256 #t)) ; input pixelprimitive
;;
;; (translate (vector 1.1 0 0))
;; ; output pixelprimitive - rendering is not active
;; ; otherwise it would overwrite the plugin output
;; (define op (build-pixels 256 256))
;;
;; ; load the FFGLTile plugin from the FreeFrame SDK
;; (define plugin (ffgl-load "FFGLTile" 256 256))
;;
;; (with-ffgl plugin
;;   (for ([i (ffgl-get-info)]) ; print plugin information
;;        (printf "~a~n" i))
;;   (printf "~a~n" (ffgl-get-parameters)) ; parameter names as strings
;;   (ffgl-process op p)) ; set destination and source pixelprimitives
;;
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


;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; Stuff for poly primitives
;; Example:
;; EndSectionDoc 

#lang scheme/base
(require "fluxus-engine.ss")
(require "building-blocks.ss")
(provide 
	vmix
	vclamp
	vsquash)

;; StartFunctionDoc-en
;; vmix a b t
;; Returns: void
;; Description:
;; Linearly interpolates the two vectors together by t
;; Example:  
;; ; mix red and blue together
;; (colour (vmix (vector 1 0 0) (vector 0 0 1) 0.5))
;; EndFunctionDoc

(define (vmix a b t)
    (vadd (vmul a t) (vmul b (- 1 t))))
	
;; StartFunctionDoc-en
;; vclamp a
;; Returns: void
;; Description:
;; Clamp the vector so the elements are all between 0 and 1
;; Example:  
;; ; make a valid colour from any old vector
;; (colour (vclamp (vector 2 400 -123)))
;; EndFunctionDoc 

(define (vclamp a)
	(define (clamp v)
		(if (< v 0) 0
			(if (> v 1) 1
				v)))
    (vector (clamp (vx a)) (clamp (vy a)) (clamp (vz a))))

;; StartFunctionDoc-en
;; vsquash a
;; Returns: void
;; Description:
;; Clamp the vector so the elements are all between 0 and 1
;; Example:  
;; ; make a valid colour from any old vector
;; (colour (vclamp (vector 2 400 -123)))
;; EndFunctionDoc 

(define (vsquash v)
	(let ((t (+ (vx v) (vy v) (vz v))))
		(vector (/ (vx v) t) (/ (vy v) t) (/ (vz v) t))))

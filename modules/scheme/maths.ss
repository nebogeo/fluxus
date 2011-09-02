;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc 

#lang racket/base
(require "fluxus-modules.ss")
(require "building-blocks.ss")
(provide 
	vmix
	vclamp
	vsquash
	hermite
	vlerp
	hermite-tangent
	lerp
	clamp
	mlerp)

;; StartFunctionDoc-en
;; vmix a b t
;; Returns: vector
;; Description:
;; Linearly interpolates the two vectors together by t
;; Example:  
;; ; mix red and blue together
;; (colour (vmix (vector 1 0 0) (vector 0 0 1) 0.5))
;; EndFunctionDoc

(define (vmix a b t)
    (vlerp a b t))
	
;; StartFunctionDoc-en
;; vclamp a
;; Returns: vector
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
;; Returns: vector
;; Description:
;; Normalise the vector so all the elements are between 0 and 1 but 
;; maintain the same ratio between them.
;; Example:  
;; ; make a valid colour from any old vector
;; (colour (vsquash (vector 2 400 -123)))
;; EndFunctionDoc 

(define (vsquash v)
	(let ((t (+ (vx v) (vy v) (vz v))))
		(vector (/ (vx v) t) (/ (vy v) t) (/ (vz v) t))))

; slow implementation of hermite curves for animation
(define (hermite p1 p2 t1 t2 s)
    ; the bernstein polynomials
    (define (h1 s)
        (+ (- (* 2 (expt s 3))
                (* 3 (expt s 2))) 1))
    
    (define (h2 s)
        (+ (* -2 (expt s 3))
            (* 3 (expt s 2))))
    
    (define (h3 s)
        (+ (- (expt s 3) (* 2 (expt s 2))) s))
    
    (define (h4 s)
        (- (expt s 3) (expt s 2)))
    
    (vadd
        (vadd 
            (vmul p1 (h1 s))
            (vmul p2 (h2 s)))
        (vadd
            (vmul t1 (h3 s))
            (vmul t2 (h4 s)))))

; slow, stupid version for getting the tangent - not in the mood for
; maths today to see how you derive it directly, must be pretty simple
(define (hermite-tangent p1 p2 t1 t2 t)
    (let ((p (hermite p1 p2 t1 t2 t)))
        (list p (vsub (hermite p1 p2 t1 t2 (- t 0.01)) p))))

;; StartFunctionDoc-en
;; lerp a b t
;; Returns: number
;; Description:
;; Linearly interpolates the two numbers together by t
;; Example:
;; (lerp 1 2 .3)
;; EndFunctionDoc

(define (lerp p1 p2 t)
	(+ p1 (* (- p2 p1) t)))
	
;; StartFunctionDoc-en
;; clamp t [low 0] [high 1]
;; Returns: number
;; Description:
;; Constrains value t to not exceed a minimum and maximum value.
;; Example:
;; (clamp 1 .2 .3)
;; EndFunctionDoc

(define (clamp t [low 0] [high 1])
	(cond [(< t low) low]
		  [(> t high) high]
		  [else t]))

;; StartFunctionDoc-en
;; vlerp a b t
;; Returns: vector
;; Description:
;; Linearly interpolates the two vectors together by t
;; Example:
;; ; mix red and blue together
;; (colour (vlerp (vector 1 0 0) (vector 0 0 1) 0.3))
;; EndFunctionDoc

(define (vlerp v1 v2 t)
	(vadd v1 (vmul (vsub v2 v1) t)))

;; StartFunctionDoc-en
;; mlerp a b t
;; Returns: vector
;; Description:
;; Linearly interpolates the two matrices together by t
;; Example:
;; (mlerp (mtranslate (vector 1 0 0)) (mtranslate (vector 0 1 0)) .3)
;; EndFunctionDoc

(define mlerp vlerp)


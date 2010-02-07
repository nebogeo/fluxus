;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc 

#lang scheme/base
(require "fluxus-modules.ss")
(require "building-blocks.ss")
(provide 
	vmix
	vclamp
	vsquash
	hermite
	vlerp
	hermite-tangent
	vlerp-tangent
	lerp
	mlerp)

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
    (vlerp a b t))
	
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

(define (lerp p1 p2 t)
    (+ (* p1 t) (* p2 (- 1 t))))
	
(define (vlerp p1 p2 t)
    (vadd (vmul p1 t) (vmul p2 (- 1 t))))

(define (vlerp-tangent p1 p2 t)
    (let ((p (vlerp t p1 p2)))
        (list p (vsub (vlerp (- t 0.01) p1 p2) p))))


(define (mlerp m1 m2 t)
    (vector 
		(lerp (vector-ref m1 0) (vector-ref m2 0) t)
		(lerp (vector-ref m1 1) (vector-ref m2 1) t)
		(lerp (vector-ref m1 2) (vector-ref m2 2) t)
		(lerp (vector-ref m1 3) (vector-ref m2 3) t)

		(lerp (vector-ref m1 4) (vector-ref m2 4) t)
		(lerp (vector-ref m1 5) (vector-ref m2 5) t)
		(lerp (vector-ref m1 6) (vector-ref m2 6) t)
		(lerp (vector-ref m1 7) (vector-ref m2 7) t)

		(lerp (vector-ref m1 8) (vector-ref m2 8) t)
		(lerp (vector-ref m1 9) (vector-ref m2 9) t)
		(lerp (vector-ref m1 10) (vector-ref m2 10) t)
		(lerp (vector-ref m1 11) (vector-ref m2 11) t)

		(lerp (vector-ref m1 12) (vector-ref m2 12) t)
		(lerp (vector-ref m1 13) (vector-ref m2 13) t)
		(lerp (vector-ref m1 14) (vector-ref m2 14) t)
		(lerp (vector-ref m1 15) (vector-ref m2 15) t)))

;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; Stuff for pixels primitives
;; Example:
;; EndSectionDoc 

#lang scheme/base
(require "fluxus-engine.ss")
(require "building-blocks.ss")
(provide 
	vmix
	vclamp
	pixels-circle
	pixels-blend-circle
	pixels-dodge
	pixels-burn
	pixels-clear)
	
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
;; pixels-circle pos radius colour
;; Returns: void
;; Description:
;; Draws a circle into a pixels primitive
;; Example:  
;; (with-primitive (build-pixels 100 100)
;;     (pixels-circle (vector 50 50 0) 30 (vector 1 0 0 1)))
;; EndFunctionDoc 

(define (pixels-circle pos radius colour)
    (pdata-index-map!
        (lambda (i c)
            (let ((p (vector (modulo i (pixels-width)) 
                             (quotient i (pixels-height)) 0)))
                (if (< (vdist p pos) radius)
                    colour
                    c)))
        "c"))

;; StartFunctionDoc-en
;; pixels-blend-circle pos radius colour
;; Returns: void
;; Description:
;; Draws a blended circle into a pixels primitive
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-blend-circle (vector 50 50 0) 30 (vector 1 0 0 1)))
;; EndFunctionDoc  

(define (pixels-blend-circle pos radius colour)
    (pdata-index-map!
        (lambda (i c)
            (let* ((p (vector (modulo i (pixels-width)) 
                              (quotient i (pixels-height)) 0))
                  (d (vdist p pos)))
                (if (< d radius)
                    (vmix c colour (/ d radius))
                    c)))
        "c"))

;; StartFunctionDoc-en
;; pixels-dodge pos radius strength
;; Returns: void
;; Description:
;; Lightens a circular area of a pixels primitive
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-dodge (vector 50 50 0) 30 (vector 1 0 0 1)))
;; EndFunctionDoc  

(define (pixels-dodge pos radius strength)
    (pdata-index-map!
        (lambda (i c)
            (let* ((p (vector (modulo i (pixels-width)) 
                              (quotient i (pixels-height)) 0))
                  (d (vdist p pos)))
                (if (< d radius)
                    (vclamp (vmix c (vadd c (vector strength strength strength)) 
						(/ d radius)))
                    c)))
        "c"))

;; StartFunctionDoc-en
;; pixels-dodge pos radius strength
;; Returns: void
;; Description:
;; Darkens a circular area of a pixels primitive
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-burn (vector 50 50 0) 30 (vector 1 0 0 1)))
;; EndFunctionDoc  

(define (pixels-burn pos radius strength)
    (pdata-index-map!
        (lambda (i c)
            (let* ((p (vector (modulo i (pixels-width)) 
                              (quotient i (pixels-height)) 0))
                  (d (vdist p pos)))
                (if (< d radius)
                    (vclamp (vmix c (vsub c (vector strength strength strength)) 
						(/ d radius)))
                    c)))
        "c"))
		
;; StartFunctionDoc-en
;; pixels-clear col
;; Returns: void
;; Description:
;; Sets all of the pixels to the supplied colour
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-clear col))
;; EndFunctionDoc  

(define (pixels-clear col)
    (pdata-map!
        (lambda (c)
            col)
        "c"))

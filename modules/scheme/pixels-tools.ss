;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc 

#lang scheme/base
(require "fluxus-engine.ss")
(require "building-blocks.ss")
(require "maths.ss")
(provide 
	pixels-circle
	pixels-blend-circle
	pixels-dodge
	pixels-burn
	pixels-clear)
		
;; StartFunctionDoc-en
;; pixels-circle pos radius colour
;; Returns: void
;; Description:
;; Draws a circle into a pixels primitive
;; Example:  
;; (with-primitive (build-pixels 100 100)
;;     (pixels-circle (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc 

(define (pixels-circle pos radius colour)
	(let ((radius (* radius radius)))
    (pdata-index-map!
        (lambda (i c)
            (let ((p (vector (modulo i (pixels-width)) 
                             (quotient i (pixels-height)) 0)))
                (if (< (vdist-sq p pos) radius)
                    colour
                    c)))
        "c")))

;; StartFunctionDoc-en
;; pixels-blend-circle pos radius colour
;; Returns: void
;; Description:
;; Draws a blended circle into a pixels primitive
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-blend-circle (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc  

(define (pixels-blend-circle pos radius colour)
	(let ((radius (* radius radius)))
    (pdata-index-map!
        (lambda (i c)
            (let* ((p (vector (modulo i (pixels-width)) 
                              (quotient i (pixels-height)) 0))
                  (d (vdist-sq p pos)))
                (if (< d radius)
                    (vmix c colour (/ d radius))
                    c)))
        "c")))

;; StartFunctionDoc-en
;; pixels-dodge pos radius strength
;; Returns: void
;; Description:
;; Lightens a circular area of a pixels primitive
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-dodge (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc  

(define (pixels-dodge pos radius strength)
	(let ((radius (* radius radius)))
    (pdata-index-map!
        (lambda (i c)
            (let* ((p (vector (modulo i (pixels-width)) 
                              (quotient i (pixels-height)) 0))
                  (d (vdist-sq p pos)))
                (if (< d radius)
                    (vclamp (vmix c (vadd c (vector strength strength strength)) 
						(/ d radius)))
                    c)))
        "c")))

;; StartFunctionDoc-en
;; pixels-burn pos radius strength
;; Returns: void
;; Description:
;; Darkens a circular area of a pixels primitive
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-burn (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc  

(define (pixels-burn pos radius strength)
	(let ((radius (* radius radius)))
    (pdata-index-map!
        (lambda (i c)
            (let* ((p (vector (modulo i (pixels-width)) 
                              (quotient i (pixels-height)) 0))
                  (d (vdist-sq p pos)))
                (if (< d radius)
                    (vclamp (vmix c (vsub c (vector strength strength strength)) 
						(/ d radius)))
                    c)))
        "c")))
		
;; StartFunctionDoc-en
;; pixels-clear col
;; Returns: void
;; Description:
;; Sets all of the pixels to the supplied colour
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-clear (vector 1 0 0))
;;     (pixels-upload))
;; EndFunctionDoc  

(define (pixels-clear col)
    (pdata-map!
        (lambda (c)
            col)
        "c"))

;; [ Copyright (C) 2009 Dave Griffiths and Evan Raskob: GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc 

#lang scheme/base
(require "fluxus.ss")


(provide 
 get-line-from-xy
 get-line-from-mouse
 mouse-pos
 world-pos
 2dvec->angle)



;; StartFunctionDoc-en
;; get-line-from-xy
;; Returns: list of 2 vectors (start position, end position)
;; Description:
;; Gets a line representing a segment of the projection of the x,y points into 3D space.
;; at depth z from the camera
;; Example:  
;; EndFunctionDoc 

(define (get-line-from-xy x y z)
    (let* ((ndcpos (vector (* (- (/ x (vx (get-screen-size))) 0.5) 2)
                    (* (- (- (/ y (vy (get-screen-size))) 0.5)) 1.5) -1))
            (scrpos2 (vtransform (vmul ndcpos z) (minverse (get-camera-transform))))
            (scrpos (vtransform ndcpos (minverse (get-camera-transform)))))
        (list scrpos scrpos2)))


;; StartFunctionDoc-en
;; get-line-from-mouse
;; Returns: list of 2 vectors (start position, end position)
;; Description:
;; Gets a line representing a segment of the projection of the mouse into 3D space.
;; Example:  
;; EndFunctionDoc 

(define (get-line-from-mouse)
   (get-line-from-xy (mouse-x mouse-y 10)))


;; StartFunctionDoc-en
;; mouse-pos
;; Returns: vector
;; Description:
;; Gets the mouse position in 3D world space.
;; Example:  
;; EndFunctionDoc 

; we'll just use the end of the projection line here
(define (mouse-pos)
    (cadr (get-line-from-mouse)))


;; StartFunctionDoc-en
;; world-pos
;; Returns: vector
;; Description:
;; Gets the world position of a point in 3D world space.
;; Example:  
;; EndFunctionDoc 

; we'll just use the end of the projection line here
(define (world-pos x y z)
    (cadr (get-line-from-xy x y z)))



;; StartFunctionDoc-en
;; 2dvec->angle x y
;; Returns: float
;; Description:
;; Converts a 2D vector into an angle, with some dodgy Dave maths
;; Example:  
;; EndFunctionDoc 

(define (2dvec->angle x y)
    (let ((q (/ 3.141 2)))
        (when (zero? y) (set! y 0.0001))
        (cond 
            [(>= y 0)
                (fmod (* (+ q q q (- q (atan (/ x y)))) 57.2957795) 360)]
            [else
                (fmod (* (+ q (- q (atan (/ x y)))) 57.2957795) 360)]
            )
        )
    )
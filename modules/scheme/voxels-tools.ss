;; [ Copyright (C) 2010 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; voxels-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc 

#lang scheme/base
(require "fluxus-modules.ss")
(require "building-blocks.ss")
(require "maths.ss")
(provide 
	voxels-index
    voxels-pos)
		
;; StartFunctionDoc-en
;; voxel-index position-vector
;; Returns: index-number
;; Description:
;; Returns the pdata index for the voxel position
;; Example:  
;; (with-primitive (build-voxels 10 10 10)
;;     (display (voxels-index (vector 5 5 5)))(newline))
;; EndFunctionDoc 

(define (voxels-index v)
    (+ (vx v) (* (vy v) (voxels-width)) (* (vz v) (voxels-width) (voxels-height))))

;; StartFunctionDoc-en
;; voxels-pos index
;; Returns: position-vector
;; Description:
;; Returns the voxel position for the given pdata index
;; Example:  
;; (with-primitive (build-voxels 10 10 10)
;;     (display (voxels-pos 200))(newline))
;; EndFunctionDoc

(define (voxels-pos i)
    (vector (modulo i (voxels-width))
            (modulo (quotient i (voxels-width)) (voxels-height))
            (quotient i (* (voxels-width) (voxels-height)))))
 
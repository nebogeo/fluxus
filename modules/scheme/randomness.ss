; Copyright (C) 2007 Dave Griffiths
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

; some random helpers

;; StartSectionDoc-en
;; randomness
;; Some helpful random functions for general use
;; Example:
;; EndSectionDoc 

(module randomness mzscheme
  (require "fluxus-engine.ss")
  
  (provide (all-defined))
  
  ;; StartFunctionDoc-en
  ;; rndf 
  ;; Returns: number
  ;; Description:
  ;; Returns a random number in the range 0->1
  ;; Example:
  ;; (display (rndf))(newline)
  ;; EndFunctionDoc 
  
  (define (rndf)
    (* (random 10000) 0.0001))
  
  ;; StartFunctionDoc-en
  ;; crndf 
  ;; Returns: number
  ;; Description:
  ;; Returns a random number in the range -1->1 (centred on zero)
  ;; Example:
  ;; (display (crndf))(newline)
  ;; EndFunctionDoc 
  
  (define (crndf)
    (* (- (rndf) 0.5) 2))
  
  ;; StartFunctionDoc-en
  ;; rndvec 
  ;; Returns: vector
  ;; Description:
  ;; Returns a random 3 element vector with each element in the range 0->1. If 
  ;; you visualise a lot of these as points, they will fill the unit cube
  ;; (see the example).
  ;; Example:
  ;; (clear)
  ;; (hint-none)
  ;; (hint-points)
  ;; (point-width 4)
  ;; (define p (build-particles 1000))
  ;; 
  ;; (show-axis 1)
  ;; 
  ;; (with-primitive p
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (vector 1 1 1))
  ;;         "c")
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (rndvec))
  ;;         "p"))
  ;; EndFunctionDoc 
  
  (define (rndvec)
    (vector (rndf) (rndf) (rndf)))
  
  ;; StartFunctionDoc-en
  ;; crndvec 
  ;; Returns: vector
  ;; Description:
  ;; Returns a random 3 element vector with each element in the range -1->1. If 
  ;; you visualise a lot of these as points, they will fill a cube centred
  ;; on the origin (see the example).
  ;; Example:
  ;; (clear)
  ;; (hint-none)
  ;; (hint-points)
  ;; (point-width 4)
  ;; (define p (build-particles 1000))
  ;; 
  ;; (show-axis 1)
  ;; 
  ;; (with-primitive p
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (vector 1 1 1))
  ;;         "c")
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (crndvec))
  ;;         "p"))
  ;; EndFunctionDoc 
  
  (define (crndvec)
    (vector (crndf) (crndf) (crndf)))
  
  ;; StartFunctionDoc-en
  ;; srndvec 
  ;; Returns: vector
  ;; Description:
  ;; Returns a random 3 element vector. If you visualise a lot of these as 
  ;; points, they will fill a sphere centred on the origin (see the example).
  ;; Example:
  ;; (clear)
  ;; (hint-none)
  ;; (hint-points)
  ;; (point-width 4)
  ;; (define p (build-particles 1000))
  ;; 
  ;; (show-axis 1)
  ;; 
  ;; (with-primitive p
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (vector 1 1 1))
  ;;         "c")
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (srndvec))
  ;;         "p"))
  ;; EndFunctionDoc 	
  
  (define (srndvec)
    (let loop ((v (crndvec)))
      (if (> (vmag v) 1) ; todo: use non sqrt version
          (loop (crndvec))
          v)))
  
  ;; StartFunctionDoc-en
  ;; hsrndvec 
  ;; Returns: vector
  ;; Description:
  ;; Returns a random 3 element vector. If you visualise a lot of these as 
  ;; points, they will cover the surface of a sphere centred on the origin 
  ;; (see the example). The name stands for "hollow sphere".
  ;; Example:
  ;; (clear)
  ;; (hint-none)
  ;; (hint-points)
  ;; (point-width 4)
  ;; (define p (build-particles 1000))
  ;; 
  ;; (show-axis 1)
  ;; 
  ;; (with-primitive p
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (vector 1 1 1))
  ;;         "c")
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (hsrndvec))
  ;;         "p"))
  ;; EndFunctionDoc 	
  
  (define (hsrndvec)
    (let loop ((v (crndvec)))
      (let ((l (vmag v)))
        (if (or (> l 1) (eq? l 0))
            (loop (crndvec))
            (vdiv v l)))))
  
  
  ;; StartFunctionDoc-en
  ;; grndf 
  ;; Returns: number
  ;; Description:
  ;; Returns a gaussian random number in the range -1->1 (centred on zero)
  ;; Example:
  ;; (display (grndf))(newline)
  ;; EndFunctionDoc 
  
  ;; gaussian
  (define (grndf)
    (let loop ((x (crndf)) (y (crndf)))
      (let ((l (+ (* x x) (* y y))))
        (if (or (>= l 1) (eq? l 0))
            (loop (crndf) (crndf))
            (* (sqrt (/ (* -2 (log l)) l)) x)))))
  
  ;; StartFunctionDoc-en
  ;; grndvec 
  ;; Returns: vector
  ;; Description:
  ;; Returns a gaussian random 3 element vector. If you visualise a lot of 
  ;; these as points, you will see a normal distribution centred on the origin. 
  ;; (see the example). 
  ;; Example:
  ;; (clear)
  ;; (hint-none)
  ;; (hint-points)
  ;; (point-width 4)
  ;; (define p (build-particles 1000))
  ;; 
  ;; (show-axis 1)
  ;; 
  ;; (with-primitive p
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (vector 1 1 1))
  ;;         "c")
  ;;     (pdata-map! 
  ;;         (lambda (p)
  ;;             (grndvec))
  ;;         "p"))
  ;; EndFunctionDoc 	
  
  (define (grndvec)
    (vector (grndf) (grndf) (grndf)))
  
  
  )

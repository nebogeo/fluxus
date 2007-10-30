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

;; StartSectionDoc-en
;; fluxus-building-blocks
;; A new and fairly untested set of higher level control structures for manipulating
;; objects and state in fluxus in a cleaner and safer manner.
;; Example:
;; EndSectionDoc 

(module fluxus-building-blocks mzscheme
  (require fluxus-engine)
  (provide 
   with-state
   with-primitive
   pdata-map!
   pdata-fold
   )
  
  ;; StartFunctionDoc-en
  ;; with-state expression ...
  ;; Returns: result of last expression
  ;; Description:
  ;; Encapsulates local state changes, and removes the need for push and pop.
  ;; Example:
  ;; ; state hierachy, by nesting with-state:
  ;; (with-state
  ;;    (hint-vertcols)
  ;;    (colour (vector 0 0 1))
  ;;    (with-state
  ;;        (transform (vector 1 0 0))
  ;;        (build-sphere 10 10))
  ;;     (build-torus 1 2 30 30))
  ;;
  ;; ; making primitives:
  ;; (define my-torus (with-state
  ;;    (hint-vertcols)
  ;;    (colour (vector 0 0 1))
  ;;    (build-torus 1 2 30 30)))
  ;; EndFunctionDoc  
  
  (define-syntax with-state
    (syntax-rules ()
      ((_ a ...)
       (begin
         (push)
         (let ((r (begin a ...)))
           (pop)
           r)))))
  
  ;; StartFunctionDoc-en
  ;; with-primitive primitive expression ...
  ;; Returns: result of last expression
  ;; Description:
  ;; Encapsulates primitive state changes, and removes the need for grab and ungrab.
  ;; Example:  
  ;; (define my-torus (with-state
  ;;    (colour (vector 0 0 1))
  ;;    (build-torus 1 2 30 30)))
  ;;
  ;; ; change the torus colour: 
  ;; (with-primtive my-torus
  ;;    (colour (vector 0 1 0)))
  ;; EndFunctionDoc  
  
  (define-syntax with-primitive
    (syntax-rules ()
      ((_ a b ...)
       (begin
         (grab a)
         (let ((r (begin b ...)))
           (ungrab)
           r)))))
  
  ;; StartFunctionDoc-en
  ;; pdata-map! procedure read/write-pdata-name read-pdata-name ...
  ;; Returns: void
  ;; Description:
  ;; A high level control structure for simplifying passing over pdata arrays for 
  ;; primitive deformation. Should be easier and less error prone than looping manually.
  ;; Writes to the first pdata array.
  ;; Example:
  ;; (clear)
  ;; (define my-torus (build-torus 1 2 30 30))
  ;; 
  ;; (with-primitive my-torus
  ;;   (pdata-map!
  ;;      (lambda (position)
  ;;          (vadd position (vector (flxrnd) 0 0))) ;; jitter the vertex in x
  ;;      "p")) ;; read/write the position pdata array
  ;; 
  ;; (with-primitive my-torus
  ;;   (pdata-map!
  ;;      (lambda (position normal)
  ;;          (vadd position normal)) ;; add the normal to the position (expand the object)
  ;;      "p" "n")) ;; read/write the position pdata array, read the normals array
  ;; EndFunctionDoc  	   
  
  (define-syntax pdata-map!
    (syntax-rules ()
      ((_ proc pdata-write-name pdata-read-name ...)
       (letrec 
           ((loop (lambda (n)
                    (cond ((not (< n 0))
                           (pdata-set! pdata-write-name n 
                                       (proc (pdata-ref pdata-write-name n) 
                                             (pdata-ref pdata-read-name n) ...))
                           (loop (- n 1)))))))
         (loop (pdata-size))))))
  
  ;; StartFunctionDoc-en
  ;; pdata-fold procedure start-value read-pdata-name ...
  ;; Returns: result of folding procedure over pdata array
  ;; Description:
  ;; A high level control structure for doing calculations on pdata arrays. 
  ;; Runs the procedure over each pdata element accumulating the result.
  ;; Should be easier and less error prone than looping manually.
  ;; Example:  
  ;; (define my-torus (build-torus 1 2 30 30))
  ;; 
  ;; ;; find the centre of the primitive by averaging 
  ;; ;; the points position's together
  ;; (let ((centre 
  ;;        (with-primitive my-torus
  ;;                        (vdiv (pdata-fold
  ;;                               vadd
  ;;                               (vector 0 0 0)
  ;;                               "p") (pdata-size)))))
  ;;   
  ;;   (display centre)(newline))
  ;; EndFunctionDoc  	   
  
  
  (define (pdata-fold p s t)
    (define (loop n)  
      (cond 
        ((< n 0) s)
        (else
         (p (pdata-ref t n) (loop (- n 1))))))
    (loop (pdata-size)))
  
  
  )

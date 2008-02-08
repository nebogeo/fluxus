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
;; obj-export
;; OBJ format model export
;; Example:
;; EndSectionDoc

; quick and dirty obj export
; really not very good - only supports triangle list polygons

(module fluxus-obj-export mzscheme
  (require "fluxus-engine.ss")
  (require (lib "fluxus-building-blocks.ss" "fluxus-0.15"))
  (provide obj-export)
  
  (define (export-prim obj file stride index)
    (define (write-indices n t p)
      (cond ((not (> n (+ t index)))
             (cond
               ((eq? p 0)
                (fprintf file "f ~a/~a/~a " 
                         (number->string n) 
                         (number->string n)
                         (number->string n)))
               ((eq? p (- stride 1))
                (fprintf file "~a/~a/~a~n"
                         (number->string n)
                         (number->string n)
                         (number->string n)))
               (else
                (fprintf file "~a/~a/~a "
                         (number->string n) 
                         (number->string n) 
                         (number->string n))))
             (write-indices (+ n 1) t (modulo (+ p 1) stride)))))
    
    (fprintf file "o fluxus-object-~a~n" (number->string obj))
    (with-primitive obj
                    (pdata-map!
                     (lambda (p)
                       (let ((tp (vtransform p (get-transform))))
                         (fprintf file "v ~a ~a ~a~n" 
                                  (number->string (vector-ref tp 0))
                                  (number->string (vector-ref tp 1)) 
                                  (number->string (vector-ref tp 2))))
                       p)
                     "p")
                    (pdata-map!
                     (lambda (n)
                       (let ((tn (vtransform-rot n (get-transform))))
                         (fprintf file "vn ~a ~a ~a~n" 
                                  (number->string (vector-ref tn 0))
                                  (number->string (vector-ref tn 1))
                                  (number->string (vector-ref tn 2))))
                       n)
                     "n")
                    (pdata-map!
                     (lambda (t)
                       (fprintf file "vt ~a ~a ~a~n" 
                                (number->string (vector-ref t 0))
                                (number->string (vector-ref t 1))
                                (number->string (vector-ref t 2)))
                       t)
                     "t")
                    (write-indices (+ index 1) (pdata-size) 0)
                    (fprintf file "\n")
                    (pdata-size)))    
  
  ;; StartFunctionDoc-en
  ;; obj-export filename-string id-list type-symbol
  ;; Returns: void 
  ;; Description: 
  ;; Exports all specified objects into one obj file.
  ;; id-list is a list of primitive ids, type-symbol is one of 'triangle-list 
  ;; or 'quad-list depending on the topology of the shapes you are exporting 
  ;; (yup, only works with one for all). Note: this is slow for heavy models
  ;; Example:
  ;; (define t (build-torus 1 2 30 30))
  ;; (obj-export "torus.obj" (list t) 'quad-list)
  ;; EndFunctionDoc			
  
  (define (obj-export filename obj-list type)      
    (let ((stride 
           (case type
             ((triangle-list) 3)
             ((quad-list) 4)
             (else (error "obj-export: unsupported geometry type"))))
           (file (open-output-file filename 'replace))
           (index 0))
      (for-each
       (lambda (obj)
         (set! index (+ index (export-prim obj file stride index))))
       obj-list)
      (close-output-port file)))
  )

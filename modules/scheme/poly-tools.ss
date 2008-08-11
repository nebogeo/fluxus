;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; Stuff for poly primitives
;; Example:
;; EndSectionDoc 

#lang scheme/base
(require "fluxus-engine.ss")
(require "building-blocks.ss")
(require "maths.ss")
(require "randomness.ss")
(provide 
	poly-type
    poly-for-each-face
    poly-for-each-triangle
    poly-for-each-tri-sample)


;; StartFunctionDoc-en
;; poly-type
;; Returns: void
;; Description:
;; Returns a symbol representing the type of the current polygon primitive.
;; primitive.
;; Example:
;; (define p (build-polygons 3 'triangle-strip))
;; (with-primitive p
;;     (display (poly-type))(newline))
;; EndFunctionDoc

(define (poly-type)	
  (let ((t (poly-type-enum)))
    (cond
      ((eq? t 0) 'triangle-strip)
      ((eq? t 1) 'quad-list)
      ((eq? t 2) 'triangle-list)
      ((eq? t 3) 'triangle-fan)
      ((eq? t 4) 'polygon))))

;; StartFunctionDoc-en
;; pdata-for-each-face proc pdatanames
;; Returns: list of pdata values
;; Description:
;; Calls proc with the indices for each face in a polygon primitive
;; Example:  
;; EndFunctionDoc 

(define (poly-for-each-face proc)

  (define (make-range n l)
  	(cond ((zero? n) '())
		(make-range (- n 1) (cons n l))))

  (let ((face '()))
    (cond ((poly-indexed?)
           (cond 
             ((eq? (poly-type) 'triangle-list)
              (for-each
               (lambda (i)   
                 (cond 
                   ((< (length face) 2)
                    (set! face (cons i face)))
                   (else
                    (proc (cons i face))
                    (set! face '()))))
               (poly-indices)))
             
             ((eq? (poly-type) 'quad-list)
              (for-each
               (lambda (i)   
                 (cond 
                   ((< (length face) 3)
                    (set! face (cons i face)))
                   (else
                    (proc (cons i face))
                    (set! face '()))))
               (poly-indices)))
             
             ((eq? (poly-type) 'triangle-strip)
              (for-each
               (lambda (i)   
                 (cond 
                   ((< (length face) 2)
                    (set! face (append face (list i))))
                   (else
                    (proc (cons i face))
                    (set! face (cdr face))))
                 (poly-indices))))
             
             ((eq? (poly-type) 'triangle-fan)
              (for-each
               (lambda (i)   
                 (cond 
                   ((< (length face) 2)
                    (set! face (append face (list i))))
                   (else
                    (proc (cons 0 face))
                    (set! face '())))
                 (poly-indices))))
             
             ((eq? (poly-type) 'polygon) (proc (poly-indices)))))
			 
          (else ; non-indexed
		  
           (cond
		   	((eq? (poly-type) 'triangle-list)
              (for ((i (in-range 0 (pdata-size))))
                 (cond 
                   ((< (length face) 2)
                    (set! face (cons i face)))
                   (else
                    (proc (cons i face))
                    (set! face '())))))
					
             ((eq? (poly-type) 'quad-list)
              (for ((i (in-range 0 (pdata-size))))
                 (cond 
                   ((< (length face) 3)
                    (set! face (append face (list i))))
                   (else
                    (proc (cons i face))
                    (set! face '())))))
             
             ((eq? (poly-type) 'triangle-strip)
              (for ((i (in-range 0 (pdata-size))))
                 (cond 
                   ((< (length face) 2)
                    (set! face (append face (list i))))
                   (else
                    (set! face (append face (list i)))
                    (proc face)
                    (set! face (cdr face))))))
             
             ((eq? (poly-type) 'triangle-fan)
              (for ((i (in-range 1 (- (pdata-size) 1))))
                (proc (list 0 i (+ i 1)))))
             
             ((eq? (poly-type) 'polygon) (proc (make-range (pdata-size) '()))))))))

;; StartFunctionDoc-en
;; pdata-for-each-triangle proc 
;; Returns: list of pdata values
;; Description:
;; Calls proc with the pdata for each triangle in a face - assumes all faces are convex. 
;; Example:  
;; EndFunctionDoc 

(define (poly-for-each-triangle proc)
  (poly-for-each-face
   (lambda (indices)
     (let ((verts (length indices)))
       (cond					
         ((eq? verts 3) (proc indices))
         ((> verts 3)
          (for ((i (in-range 1 (- verts 1))))
               (proc (list (list-ref indices 0) 
			   			   (list-ref indices i)
						   (list-ref indices (+ i 1))))))
         (else
          (error "poly-for-each-triangle: faces with less than 3 verts!")))))))

;; StartFunctionDoc-en
;; pdata-for-each-tri-sample proc samples-per-triangle
;; Returns: void
;; Description:
;; Calls proc with the triangle indices and a random barycentric coord. 
;; Example:  
;; EndFunctionDoc 

(define (poly-for-each-tri-sample proc samples-per-triangle)
  (poly-for-each-triangle 
   (lambda (indices)
     (for ((x (in-range 0 samples-per-triangle)))
          (proc indices (vsquash (rndvec)))))))


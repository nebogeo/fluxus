;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc 

#lang racket/base
(require "fluxus-modules.ss")
(require "building-blocks.ss")
(require "maths.ss")
(require "randomness.ss")
(require "shapes.ss")
(provide 
	poly-type
    poly-for-each-face
    poly-for-each-triangle
    poly-for-each-tri-sample
	poly-build-triangulate
	build-extrusion
	build-partial-extrusion
	partial-extrude
	build-disk)


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
;; poly-for-each-face proc pdatanames
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
                    (set! face (cdr face)))))
                 (poly-indices)))
             
             ((eq? (poly-type) 'triangle-fan)
              (for-each
               (lambda (i)   
                 (cond 
                   ((< (length face) 2)
                    (set! face (append face (list i))))
                   (else
                    (proc (cons 0 face))
                    (set! face '()))))
                 (poly-indices)))
             
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
;; poly-for-each-triangle proc
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
;; poly-build-triangulate primitive-id 
;; Returns: primitive-id
;; Description:
;; A new poly primitive of type triangle-list representing the supplied poly primitive.
;; Example:  
;; (define triangulated-plane (poly-build-triangulate (build-seg-plane 20 20)))
;; EndFunctionDoc 
		  
(define (poly-build-triangulate id)
  (with-primitive id
    (let ((triangles '()))
      (poly-for-each-triangle
	    (lambda (indices)
	      (set! triangles (cons 
		     (map
			   (lambda (name)
			   	  (list 
				    name
				    (pdata-ref name (list-ref indices 0))
				    (pdata-ref name (list-ref indices 1))
				    (pdata-ref name (list-ref indices 2))))
			 (pdata-names)) triangles))))
	(set! triangles (reverse triangles))
	(let ((r (build-polygons (* (length triangles) 3) 'triangle-list))
	      (pos 0))	
      (with-primitive r  
		(for-each
		  (lambda (triangle)
	    	(for-each 
			  (lambda (type)
		    	(pdata-set! (car type) (* pos 3) (list-ref type 1))
		    	(pdata-set! (car type) (+ (* pos 3) 1) (list-ref type 2))
		    	(pdata-set! (car type) (+ (* pos 3) 2) (list-ref type 3)))
			  triangle)
			 (set! pos (+ pos 1)))
		  triangles) r)))))
		  

;; StartFunctionDoc-en
;; poly-for-each-tri-sample proc samples-per-triangle
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

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; extrusion code

(define (draw-profile index profile offset)
    (cond ((not (null? profile))
            (pdata-set! "p" (inexact->exact (floor index)) (vadd (car profile) offset))
            (draw-profile (+ index 1) (cdr profile) offset))))

(define (transform-profile profile m)
    (cond
        ((null? profile) '())
        (else
            (cons (vtransform (car profile) m) 
                (transform-profile (cdr profile) m)))))

; figures out the vector for rotation of the profile
(define (path-vector first-segment path lv)
    (let* ((v (if (or (null? path) (null? (cdr path)))  ; last segment?
                    lv                      ; use the last vector used
                    (vsub (cadr path) (car path)))) ; use the next point
            (vd (if first-segment v          ; first segment?
                    (vadd (vmul lv 0.5) ; blend with the last vector
                        (vmul v 0.5)))))
        vd))

(define (extrude-segment index profile path width lv up size)
    (if (not (null? path))
            (let ((v (path-vector (zero? index) path lv)))
                (draw-profile index (transform-profile profile 
                        (mmul
                            (maim v up)
                            (mrotate (vector 0 90 0))
                            (mscale (vmul (vector (car width) (car width) (car width)) size))))
                    (car path))
                v)
		lv))

(define (extrude index profile path width lv up)
    (cond ((not (null? path))
            (let ((v (extrude-segment index profile path width lv up 1)))
                (extrude (+ index (length profile)) profile (cdr path) (cdr width) v up)))))

(define (stitch-face index count profile-size in)
    (cond 
        ((eq? 1 count) 
            (append in (list (+ (- index profile-size) 1) index (+ index profile-size) 
                    (+ (- index profile-size) 1 profile-size))))
        (else
            (append 
                (list (+ index 1) index
                    (+ index profile-size) (+ index profile-size 1))
                (stitch-face (+ index 1) (- count 1) profile-size in)))))

(define (stitch-indices index profile-size path-size in)
    (cond 
        ((eq? 1 path-size) in)
        (else
            (append
                (stitch-face index profile-size profile-size '())
                (stitch-indices (+ index profile-size) 
                    profile-size
                    (- path-size 1)
                    in)))))

(define (build-tex-coords profile-size path-size vscale)
    (pdata-index-map!
        (lambda (i t)
            (vector (* vscale (/ (quotient i profile-size) path-size))
                (/ (modulo i profile-size) profile-size) 0))
        "t"))

;; StartFunctionDoc-en
;; build-extrusion profile-list path-list width-list tex-vscale up
;; Returns: primitive-id
;; Description:
;; Returns an indexed polygon primitive made by extruding the profile along path and scaling using values in width.
;; The path and width lists need to be the same size. tex-vscale allows you to scale the texture coordinates along the 
;; length of the extrusion. An up vector is needed for aiming the profile along the path.
;; Example:  
;; (clear)
;; (build-extrusion 
;;     (build-circle-points 20 0.3)
;;     (list
;;         (vector 0 0 0)
;;         (vector 0 1 2)
;;         (vector 0 -1 4)
;;         (vector 0 0 6))
;;     (list 0 1 1 0) 1 (vector 0 1 0))
;; EndFunctionDoc 

(define (build-extrusion profile path width tex-vscale up)
    (let ((p (build-polygons (* (length profile) (length path)) 'quad-list)))
        (with-primitive p
            (poly-set-index (stitch-indices 0 (length profile) (length path) '()))
            (build-tex-coords (length profile) (length path) tex-vscale)
            (extrude 0 profile path width (vector 0 0 0) up)
            (recalc-normals 0))
        p))

;; StartFunctionDoc-en
;; build-partial-extrusion profile-list path-list tex-vscale
;; Returns: primitive-id
;; Description:
;; Builds a primitive ready to be used with partial-extrusion. 
;; Use this is for animating growth along extrusions.
;; Example:  
;; (clear)
;; 
;; (define profile (build-circle-points 10 0.3))
;; (define path (build-list 20 (lambda (i) (vector (crndf) (crndf) i))))
;; (define width (build-list 20 (lambda (_) 1)))
;; 
;; (hint-wire)
;; (define p (build-partial-extrusion profile path 1))
;;  
;; (every-frame 
;;     (with-primitive p
;;         (partial-extrude (* (length path) 0.5 (+ (sin (time)) 1)) 
;;             profile path width (vector 0 1 0) 0.1))) 
;; EndFunctionDoc

(define (build-partial-extrusion profile path tex-vscale)
    (let ((p (build-polygons (* (length profile) (length path)) 'quad-list)))
        (with-primitive p
            (poly-set-index (stitch-indices 0 (length profile) (length path) '()))
            (build-tex-coords (length profile) (length path) tex-vscale))
        p))


;; StartFunctionDoc-en
;; partial-extrude profile-list t profile-list path-list width-list up grow-value
;; Returns: primitive-id
;; Description:
;; Animates growth along extrusions. t is a value between 0 and the length of the path, 
;; and controls how far along the extrusion to calculate. Grow value is a scale to control
;; how much the profile is scaled to change it's width as it grows.
;; Example:  
;; (clear)
;; 
;; (define profile (build-circle-points 10 0.3))
;; (define path (build-list 20 (lambda (i) (vector (crndf) (crndf) i))))
;; (define width (build-list 20 (lambda (_) 1)))
;; 
;; (hint-wire)
;; (define p (build-partial-extrusion profile path 100))
;;  
;; (every-frame 
;;     (with-primitive p
;;         (partial-extrude (* (length path) 0.5 (+ (sin (time)) 1)) 
;;             profile path width (vector 0 1 0) 0.1))) 
;; EndFunctionDoc

(define (partial-extrude t profile path width up grow)
    (define (chop-front l n)
        (cond ((null? l) l)
            (else
                (if (zero? n) (cons (car l) (chop-front (cdr l) n))
                    (chop-front (cdr l) (- n 1))))))
    
    (define (collapse-front t)
        (let ((start (* (floor t) (length profile))))
            (for ((i (in-range (+ start (* (length profile) 1)) (pdata-size))))
                (pdata-set! "p" (inexact->exact (floor i)) (pdata-ref "p" (inexact->exact (floor start)))))))
    
    (define (scale-front t)
        (when (> t 1)
            (let* ((start (* (floor t) (length profile)))
                    (from (list-ref path (- (inexact->exact (floor t)) 1)))
                    (to (list-ref path (+ (inexact->exact (floor t)) 0))))
                
                (for ((i (in-range start (+ start (length profile)))))
                     (pdata-set! "p" (inexact->exact (floor i)) (vmix (vmix from to (- t (floor t)))
                                                                      (pdata-ref "p" (inexact->exact (floor i)))
                                                                      (- t (floor t))))))))

    (define (_ t v g)
        (cond 
            ((< t 0) (recalc-normals 0) v)
            (else
                (let* ((tc (min t (- (length path) 1))) 
					   (start (* (floor tc) (length profile))))
                    (_ (- t 1)
                        (extrude-segment start profile
                            (chop-front path (floor tc))
                            (chop-front width (floor tc)) v up 
                            (if (< g 1)
                                (+ g (* (- t (floor t)) grow))
                                g))
                        (if (< g 1)
                            (+ g grow)
                            1))))))
    (_ t (vector 0 0 0) 0)
    (scale-front (min t (- (length path) 1)))
    (collapse-front (min t (- (length path) 1))))

;; StartFunctionDoc-en
;; build-disk num-points
;; Returns: primitive-id
;; Description:
;; Builds a disk shaped poly primitive
;; Example:  
;; (clear)
;; (build-disk 10)
;; EndFunctionDoc 

(define (build-disk n)
  (let ((p (build-polygons n 'polygon))
        (points (reverse (build-circle-points n 1))))
  	(with-primitive p
	  (pdata-index-map!
	    (lambda (i p)
		  (list-ref points i))
		"p")
	   (pdata-map! ; set the texture coordinate to be planar
	   		(lambda (t p)
				(vadd p (vector 0.5 0.5 0)))
		"t" "p")
		(pdata-map!
			(lambda (n)
			  (vector 0 0 1))
			"n"))
	p))


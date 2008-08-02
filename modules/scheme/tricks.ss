;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

#lang scheme/base
(require scheme/class)
(require fluxus-015/fluxus)

(provide 
 expand
 cheap-toon
 occlusion-texture-bake)

; expand object along the normals
(define (expand distance)
  (pdata-map! 
   (lambda (p n)
     (vadd p (vmul n distance)))
   "p" "n"))

; apply a bargain basement toon outline effect
; attaches a copy of the object which is expanded and flipped inside out
; so area around edges becomes the unlit colour of the expanded object
(define (cheap-toon obj pen-width pen-colour)
  (with-state
	  ; copy and parent a new object
	  (parent obj)
          (with-primitive (with-primitive obj (build-copy obj))
                          ; setup toon appearance
		  				  ;(poly-convert-to-indexed)
                          (hint-unlit)
                          (colour pen-colour)
                          ; grow and flip object inside out
                          (expand pen-width)
                          (hint-cull-ccw)))
  ;(with-primitive obj (recalc-normals 0))
  )

; proper ambient occlusion texture bake (very slow)
(define (occlusion-texture-bake tex prim samples-per-face rays-per-sample burn-width burn-strength)
    (define (vsquash v)
        (let ((t (+ (vx v) (vy v) (vz v))))
            (vector (/ (vx v) t) (/ (vy v) t) (/ (vz v) t))))

    ; return a line on the hemisphere 
    (define (rnd-hemi-point n)
        (let loop ((v (crndvec)))
            (if (> (vdot n v) 0)
                (vmul v 10)
                (loop (crndvec)))))                

    (define (make-sample-hemi n count l)
        (cond 
            ((zero? count) l)
            (else
                (make-sample-hemi n (- count 1) (cons (rnd-hemi-point n) l)))))
       
    (define (sample-rnd-face-point tri b)
        (let 
             ((pos (vadd
                       (vadd (vmul (list-ref (list-ref tri 0) 0) (vx b))
                             (vmul (list-ref (list-ref tri 1) 0) (vy b)))
                             (vmul (list-ref (list-ref tri 2) 0) (vz b))))
              (norm (vadd
                       (vadd (vmul (list-ref (list-ref tri 0) 1) (vx b))
                             (vmul (list-ref (list-ref tri 1) 1) (vy b)))
                             (vmul (list-ref (list-ref tri 2) 1) (vz b)))))
              (foldl
                  (lambda (point r)                     

                      (let ((a (vadd pos (vmul norm 0.04)))
                            (b (vadd pos point)))

                      ; visualise the rays
                      (let ((l (with-state
                        (concat (with-primitive prim (get-transform)))
                        (hint-none)
                        (hint-unlit)
                        (hint-wire)
                        (wire-opacity 0.1)
                        (build-ribbon 2))))
                        (with-primitive l
                            (pdata-set! "p" 0 a)
                            (pdata-set! "p" 1 b)))

                      (if (not (null? (line-intersect a b)))
                          (* r 0.95)
                          r)))
                  1
                  (make-sample-hemi norm rays-per-sample '()))))
 

    (let ((w (with-primitive tex (pixels-width)))
          (h (with-primitive tex (pixels-height))))
        (with-primitive prim
            (texture (pixels->texture tex))
            (poly-for-each-face
                (lambda (face-data)
                    (for ((x (in-range 0 samples-per-face)))
                        (let* ((bary (vsquash (vmul (rndvec) 4)))
                               (v (sample-rnd-face-point face-data bary))
                               (tc (vadd
                                   (vadd (vmul (list-ref (list-ref face-data 0) 2) (vx bary))
                                         (vmul (list-ref (list-ref face-data 1) 2) (vy bary)))
                                         (vmul (list-ref (list-ref face-data 2) 2) (vz bary))))
                               (tu (inexact->exact (round (* (vector-ref tc 0) w))))
                               (tv (inexact->exact (round (* (vector-ref tc 1) h)))))
                    
                        (with-primitive tex
                            (when (< v 1)
                                (pixels-burn (vector tu tv 0) burn-width (* v burn-strength)))))))
                (list "p" "n" "t")))))

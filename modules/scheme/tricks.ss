;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

#lang scheme/base
(require scheme/class)
(require fluxus-015/fluxus)

(provide 
 expand
 reverse-winding-order
 cheap-toon)

; expand object along the normals
(define (expand distance)
  (pdata-map! 
   (lambda (p n)
     (vadd p (vmul n distance)))
   "p" "n"))

; flip the winding order of triangle poly primitives
(define (reverse-winding-order)
  (define (inner n)
    (let ((temp (pdata-ref "p" n)))
      (pdata-set! "p" n (pdata-ref "p" (+ n 2)))
      (pdata-set! "p" (+ n 2) temp))
    (if (<= n 0)
        0
        (inner (- n 3))))
  (inner (pdata-size)))

; apply a bargain basement toon outline effect
; attaches a copy of the object which is expanded and flipped inside out
; so area around edges becomes the unlit colour of the expanded object
(define (cheap-toon obj pen-width pen-colour)
  (with-state
	  ; copy and parent a new object
	  (parent obj)
          (with-primitive (with-primitive obj (build-copy obj))
                          ; setup toon appearance
                          (hint-unlit)
                          (colour pen-colour)
                          ; grow and flip object inside out
                          (expand pen-width)
                          (reverse-winding-order)))
  (with-primitive obj (recalc-normals 0)))

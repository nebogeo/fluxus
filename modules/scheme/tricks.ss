;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

#lang scheme/base
(require scheme/class)
(require fluxus-015/fluxus)

(provide 
 expand
 cheap-toon)

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

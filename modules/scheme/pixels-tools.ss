;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc 

;; StartSectionDoc-fr
;; scheme-utils
;; Commandes fluxus haut-niveau écrites en Scheme.
;; Exemple:
;; EndSectionDoc 

#lang racket/base
(require "fluxus-modules.ss")
(require "building-blocks.ss")
(require "maths.ss")
(provide 
	pixels-circle
	pixels-blend-circle
	pixels-dodge
	pixels-burn
	pixels-clear
    pixels-index
    pixels-texcoord)
		
;; StartFunctionDoc-en
;; pixels-circle pos radius colour
;; Returns: void
;; Description:
;; Draws a circle into a pixels primitive
;; Example:  
;; (with-primitive (build-pixels 100 100)
;;     (pixels-circle (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc 

;; StartFunctionDoc-fr
;; pixels-circle pos rayon couleur
;; Retour: vide
;; Description:
;; Dessine un cercle dans une primitive pixels.
;; Exemple:
;; (with-primitive (build-pixels 100 100)
;;     (pixels-circle (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc 

(define (pixels-circle pos radius colour)
	(let ((radius (* radius radius)))
    (pdata-index-map!
        (lambda (i c)
            (let ((p (vector (modulo i (pixels-width)) 
                             (quotient i (pixels-height)) 0)))
                (if (< (vdist-sq p pos) radius)
                    colour
                    c)))
        "c")))

;; StartFunctionDoc-en
;; pixels-blend-circle pos radius colour
;; Returns: void
;; Description:
;; Draws a blended circle into a pixels primitive
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-blend-circle (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc  

;; StartFunctionDoc-fr
;; pixels-blend-circle pos rayon couleur
;; Retour: vide
;; Description:
;; Dessine un cercle mélangé à la primitive pixels.
;; Exemple: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-blend-circle (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc  

(define (pixels-blend-circle pos radius colour)
  (let ((radius (* radius radius)))
    (pdata-index-map!
     (lambda (i c)
       (let* ((p (vector (modulo i (pixels-width)) 
                         (quotient i (pixels-height)) 0))
              (d (vdist-sq p pos)))
         (if (< d radius)
             (vmix c colour (/ d radius))
             c)))
     "c")))

;; StartFunctionDoc-en
;; pixels-dodge pos radius strength
;; Returns: void
;; Description:
;; Lightens a circular area of a pixels primitive
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-dodge (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc  

;; StartFunctionDoc-fr
;; pixels-dodge pos rayon force
;; Retour: vide
;; Description:
;; Illumine une zone circulaire dans la primitive pixels.
;; Exemple:
;; (with-primitive (build-pixels 100 100)
;;     (pixels-dodge (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc  

(define (pixels-dodge pos radius strength)
	(let ((radius (* radius radius)))
    (pdata-index-map!
        (lambda (i c)
            (let* ((p (vector (modulo i (pixels-width)) 
                              (quotient i (pixels-height)) 0))
                  (d (vdist-sq p pos)))
                (if (< d radius)
                    (vclamp (vmix c (vadd c (vector strength strength strength)) 
						(/ d radius)))
                    c)))
        "c")))

;; StartFunctionDoc-en
;; pixels-burn pos radius strength
;; Returns: void
;; Description:
;; Darkens a circular area of a pixels primitive
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-burn (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc  

;; StartFunctionDoc-fr
;; pixels-burn pos rayon force
;; Retour: vide
;; Description:
;; Assombri une zone circulaire dans la primitive pixels.
;; Exemple:
;; (with-primitive (build-pixels 100 100)
;;     (pixels-burn (vector 50 50 0) 30 (vector 1 0 0 1))
;;     (pixels-upload))
;; EndFunctionDoc  

(define (pixels-burn pos radius strength)
	(let ((radius (* radius radius)))
    (pdata-index-map!
        (lambda (i c)
            (let* ((p (vector (modulo i (pixels-width)) 
                              (quotient i (pixels-height)) 0))
                  (d (vdist-sq p pos)))
                (if (< d radius)
                    (vclamp (vmix c (vsub c (vector strength strength strength)) 
						(/ d radius)))
                    c)))
        "c")))
		
;; StartFunctionDoc-en
;; pixels-clear col
;; Returns: void
;; Description:
;; Sets all of the pixels to the supplied colour
;; Example: 
;; (with-primitive (build-pixels 100 100)
;;     (pixels-clear (vector 1 0 0))
;;     (pixels-upload))
;; EndFunctionDoc  

;; StartFunctionDoc-fr
;; pixels-clear couleur-vecteur
;; Retour: vide
;; Description:
;; Tourne tout les pixels en la couleur demandée.
;; Exemple:
;; (with-primitive (build-pixels 100 100)
;;     (pixels-clear (vector 1 0 0))
;;     (pixels-upload))
;; EndFunctionDoc  

(define (pixels-clear col)
    (pdata-map!
        (lambda (c)
            col)
        "c"))

;; StartFunctionDoc-en
;; pixels-index position-vector
;; Returns: index-number
;; Description:
;; Returns the pdata index for the given texture coordinate
;; Example:  
;; (with-primitive (build-pixels 10 10)
;;     (display (pixels-index (vector 0.5 0.5 0)))(newline))
;; EndFunctionDoc 

;; StartFunctionDoc-fr
;; pixels-index position-vecteur
;; Retour: index-nombre
;; Description:
;; Retourne l'index pdata pour les coordonnées de texture fournies.
;; Exemple: 
;; (with-primitive (build-pixels 10 10)
;;     (display (pixels-index (vector 0.5 0.5 0)))(newline))
;; EndFunctionDoc 

(define (pixels-index v)
  (+ (inexact->exact (floor (* (vx v) (pixels-width))))
     (* (inexact->exact (floor (* (vy v) (pixels-width)))) (pixels-height))))

;; StartFunctionDoc-en
;; pixels-texcoord index
;; Returns: position-vector
;; Description:
;; Returns the texture coordinate for the given pdata index
;; Example:  
;; (with-primitive (build-pixels 10 10)
;;     (display (pixels-texcoord 25))(newline))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; pixels-texcoord index
;; Retour: position-vecteur
;; Description:
;; Retourne les coordonnées de textures pour l'index pdata donné.
;; Exemple:
;; (with-primitive (build-pixels 10 10)
;;     (display (pixels-texcoord 25))(newline))
;; EndFunctionDoc

(define (pixels-texcoord i)
    (vector (/ (modulo i (pixels-width)) (pixels-width))
            (/ (quotient i (pixels-width)) (pixels-width))
            0))

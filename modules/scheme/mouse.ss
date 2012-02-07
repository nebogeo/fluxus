;; [ Copyright (C) 2009 Dave Griffiths and Evan Raskob: GPLv2 see LICENCE ]

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
(require "fluxus.ss")


(provide 
 get-line-from-xy
 world-pos
 mouse-pos
 mouse-pos-z
 2dvec->angle)



;; StartFunctionDoc-en
;; get-line-from-xy
;; Returns: list of 2 vectors (start position, end position)
;; Description:
;; Gets a line representing a segment of the projection of the x,y points into 3D space.
;; at depth z from the camera
;; Example:  
;; EndFunctionDoc 

;; StartFunctionDoc-fr
;; get-line-from-xy
;; Retour: liste de 2 vecteurs (position début, position fin)
;; Description:
;; Ligne représentant un segment de la projection des points x, y dans l'espace 3D
;; à la distance z de la caméra.
;; Exemple:  
;; EndFunctionDoc 

(define (get-line-from-xy x y z)
    (let* ((ndcpos (vector (* (- (/ x (vx (get-screen-size))) 0.5) 2)
                    (* (- (- (/ y (vy (get-screen-size))) 0.5)) 1.5) -1))
            (scrpos2 (vtransform (vmul ndcpos z) (minverse (get-camera-transform))))
            (scrpos (vtransform ndcpos (minverse (get-camera-transform)))))
        (list scrpos scrpos2)))


;; StartFunctionDoc-en
;; world-pos
;; Returns: vector
;; Description:
;; Gets the world position of a point in 3D world space.
;; Example:  
;; EndFunctionDoc 

;; StartFunctionDoc-fr
;; world-pos x-nombre y-nombre z-nombre
;; Retour: vecteur
;; Description:
;; Retourne la position d'un point dans l'espace 3D par rapport à la caméra.
;; Gets the world position of a point in 3D world space.
;; Exemple:
;; EndFunctionDoc 

; we'll just use the end of the projection line here
(define (world-pos x y z)
    (cadr (get-line-from-xy x y z)))



;; StartFunctionDoc-en
;; mouse-pos
;; Returns: vector
;; Description:
;; Gets the mouse position in 3D world space.
;; Example:  
;; EndFunctionDoc 

;; StartFunctionDoc-fr
;; mouse-pos
;; Retour: vecteur
;; Description:
;; Retourne la position de la souris dans l'espace 3D.
;; Exemple:  
;; EndFunctionDoc 

; we'll just use the end of the projection line here
(define (mouse-pos)
    (world-pos (mouse-x) (mouse-y) 10))


;; StartFunctionDoc-en
;; mouse-pos-z
;; Returns: vector
;; Description:
;; Gets the mouse position in 3D world space at depth z.
;; Example:  
;; EndFunctionDoc 

;; StartFunctionDoc-fr
;; mouse-pos-z
;; Retour: vecteur
;; Description:
;; Retourne la position de la souris dans l'espace 3D avec z comme profondeur.
;; Exemple:
;; EndFunctionDoc 

; we'll just use the end of the projection line here
(define (mouse-pos-z z)
    (world-pos (mouse-x) (mouse-y) z))


;; StartFunctionDoc-en
;; 2dvec->angle x y
;; Returns: float
;; Description:
;; Converts a 2D vector into an angle, with some dodgy Dave maths
;; Example:  
;; EndFunctionDoc 

;; StartFunctionDoc-fr
;; 2dvec->angle x y
;; Retour: décimal
;; Description:
;; Convertit un vecteur 2D en un angle, avec quelques maths louches de Dave.
;; Exemple:
;; EndFunctionDoc 

(define (2dvec->angle x y)
    (let ((q (/ 3.1415 2)))
        (when (zero? y) (set! y 0.0000001))
        (cond 
            [(>= y 0)
                (fmod (* (+ q q q (- q (atan (/ x y)))) 57.2957795) 360)]
            [else
                (fmod (* (+ q (- q (atan (/ x y)))) 57.2957795) 360)]
            )
        )
    )

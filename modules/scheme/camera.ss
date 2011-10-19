;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scratchpad
;; Functions available as part of the fluxus scratchpad.
;; Example:
;; EndSectionDoc

;; StartSectionDoc-pt
;; scratchpad
;; Funções disponíveis como parte do scratchpad fluxus.
;; EndSectionDoc

;; StartSectionDoc-fr
;; scratchpad
;; Fonctions disponibles dans le cadre de scratchpad Fluxus
;; Exemple:
;; EndSectionDoc

#lang racket/base

(require "fluxus-modules.ss")
(require "input.ss")
(require "building-blocks.ss")
(provide
 input-camera
 set-camera-transform
 get-camera-transform
 reset-camera
 unlock-camera
 set-camera-position
 set-fov)

(define camera-locked #f)
(define camera-matrix (mtranslate (vector 0 1 -10)))
(define camera-position (vector 0 0 -10))
(define camera-rot-now (vector 0 0 0 1))
(define camera-rot-start (vector 0 0 0 1))
(define click-mouse (vector 0 0))
(define last-mouse (vector 0 0))
(define last-button 0)

(define (unlock-camera)
  (set! camera-locked #f))

;; StartFunctionDoc-en
;; reset-camera
;; Returns: void
;; Description:
;; Resets the camera transform, useful if it becomes trashed, or you get lost
;; somewhere in space. Also turns off camera locking to objects with (lock-camera)
;; Example:
;; ; ruin the camera transform
;; (set-camera-transform (vector 123 41832 28 0.2 128 0.001 123 41832 28 0.2 128 0.001 0.2 100 13 1931))
;; ; set it back to the starting position/orientation
;; (reset-camera)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; reset-camera
;; Retorna: void
;; Descrição:
;; Reinicia a transformação da câmera, útil se ela quebra, ou se você
;; se perde em algum lugar do espaço. Também desliga o travamento de
;; câmera com (lock-camera)
;; Exemplo:
;; ; ruin the camera transform
;; (set-camera-transform (vector 123 41832 28 0.2 128 0.001 123 41832 28 0.2 128 0.001 0.2 100 13 1931))
;; ; set it back to the starting position/orientation
;; (reset-camera)
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; reset-camera
;; Retour: vide
;; Description:
;; Réinitialise les transformations de la caméra, utile si la vue deviens confuse,
;; ou si elle est perdue quelque part dans l'espace. Détache également l'accroche de la caméra
;; sur les objets par (lock-camera)
;; Exemple:
;; ; ruine la transformation de la caméra
;; (set-camera-transform (vector 123 41832 28 0.2 128 0.001 123 41832 28 0.2 128 0.001 0.2 100 13 1931))
;; ; La replace en postion/orientation de départ
;; (reset-camera)
;; EndFunctionDoc

(define (reset-camera)
  (set! camera-matrix (mtranslate (vector 0 1 -10)))
  (set! camera-position (vector 0 0 -10))
  (set! camera-rot-now (vector 0 0 0 1))
  (set! camera-rot-start (vector 0 0 0 1))
  (set! camera-locked #f)
  (update-camera))

;; StartFunctionDoc-en
;; set-camera-transform transform-matrix
;; Returns: void
;; Description:
;; Overrides and locks the camera transform with your own.
;; To unlock again call reset-camera
;; Example:
;; (set-camera-transform (mtranslate (vector 0 0 -10)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; set-camera-transform matrix-transformação
;; Retorna: void
;; Descrição:
;; Substitui e trava a transformação da câmera com a sua própria.
;; Para destravar de novo chame reset-camera
;; Exemplo:
;; (set-camera-transform (mtranslate (vector 0 0 -10)))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; set-camera-transform matrice-de-transformation
;; Retour: vide
;; Description:
;; Réassigne et bloque la transformation de la caméra avec votre valeur.
;; Pour la débloquer à nouveau, rappeler (reset-camera)
;; Exemple:
;; (set-camera-transform (mtranslate (vector 0 0 -10)))
;; EndFunctionDoc

(define (set-camera-transform m)
  (set! camera-locked #t)
  (set! camera-matrix m))

(define (set-camera-position s)
  (set! camera-position s)
  (set! camera-matrix (mtranslate s)))

;; StartFunctionDoc-en
;; get-camera-transform
;; Returns: transform-matrix
;; Description:
;; Returns the current camera transform.
;; To unlock again call reset-camera
;; Example:
;; (define tx (get-camera-transform))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; get-camera-transform
;; Retorna: matrix-transformação
;; Descrição:
;; Retorna a transformação da câmera atual.
;; Para destravar de novo chame camera-reset.
;; Exemplo:
;; (define tx (get-camera-transform))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; get-camera-transform
;; Retour: matrice-de-transformation
;; Description:
;; Retourne la transformation de la caméra en cours.
;; Pour la débloquer à nouveau, rappeler (reset-camera)
;; Exemple:
;; (define tx (get-camera-transform))
;; EndFunctionDoc

(define (get-camera-transform)
  camera-matrix)

; schemified from Artem's code in FluxusMain.cpp
(define (on-unit-sphere mx my)
  (let ((mag (+ (* mx mx) (* my my))))
    (cond
      ((> mag 1.0)
       (let ((scale (/ 1.0 (sqrt mag))))
         (vector (* mx scale) (* my scale) 0)))
      (else
       (vector mx my (sqrt (- 1 mag)))))))


(define (input-camera key button special state x y mod width height)
  (when (or (mouse-button 1) (mouse-button 2) (mouse-button 3))
    (cond
      ((zero? state) ; click
       (vector-set! last-mouse 0 x)
       (vector-set! last-mouse 1 y)
       (set! last-button button)
       (set! camera-rot-start
             (qmul camera-rot-now camera-rot-start))
       (set! camera-rot-now (vector 0 0 0 1))
       (vector-set! click-mouse 0    (- (/ x (/ (- width  1) 2)) 1))
       (vector-set! click-mouse 1 (- (- (/ y (/ (- height 1) 2)) 1))))
      (else ; drag
       (cond
         ((eq? last-button 0) ; button 1
          (let ((d (on-unit-sphere (vector-ref click-mouse 0)
                                   (vector-ref click-mouse 1)))
                (m (on-unit-sphere (- (/ x (/ (- width  1) 2)) 1)
                                   (- (- (/ y (/ (- height 1) 2)) 1)))))
            (vector-set! camera-rot-now 0 (- (* (vector-ref d 1) (vector-ref m 2))
                                             (* (vector-ref d 2) (vector-ref m 1))))
            (vector-set! camera-rot-now 1 (- (* (vector-ref d 2) (vector-ref m 0))
                                             (* (vector-ref d 0) (vector-ref m 2))))
            (vector-set! camera-rot-now 2 (- (* (vector-ref d 0) (vector-ref m 1))
                                             (* (vector-ref d 1) (vector-ref m 0))))
            (vector-set! camera-rot-now 3 (+ (* (vector-ref d 0) (vector-ref m 0))
                                             (* (vector-ref d 1) (vector-ref m 1))
                                             (* (vector-ref d 2) (vector-ref m 2))))

            (set! camera-rot-now (qnormalise camera-rot-now))))
         ((eq? last-button 1) ; button 2
          (vector-set! camera-position 0 (+ (vector-ref camera-position 0)
                                            (/ (- x (vector-ref last-mouse 0)) 50.0)))
          (vector-set! camera-position 1 (+ (vector-ref camera-position 1)
                                            (- (/ (- y (vector-ref last-mouse 1)) 50.0)))))
         ((eq? last-button 2) ; button 3
          (vector-set! camera-position 2 (+ (vector-ref camera-position 2)
                                            (- (/ (- y (vector-ref last-mouse 1)) 50.0))))))
       (vector-set! last-mouse 0 x)
       (vector-set! last-mouse 1 y)
       (when (not camera-locked) (update-camera))))))

(define (set-fov fovy near far)
  ;; Specify vertical FOV in degrees and clip info to make sure it's consistent
  (let* ([ymax (* near (tan (* (/ (* fovy 3.141592) 180) 0.5)))]
         [ymin (- ymax)]
         [scrsize (get-screen-size)]
         [aspect (/ (vector-ref scrsize 0) (vector-ref scrsize 1))]     ;; width/height
         )
    ;(set! last-fovy fovy)       ;; remember so we can handle reshapes
    ;(set! last-near near)
    ;(set! last-far far)
    (clip near far)
    (frustum (* ymin aspect) (* ymax aspect) ymin ymax)))

(define (update-camera)
  (set! camera-matrix
        (mmul (mtranslate camera-position)
              (mtranspose (qtomatrix (qconjugate
                              (qmul camera-rot-now
                                    camera-rot-start))))))
  (set-ortho-zoom (vector-ref camera-position 2)))

; set the initial camera matrix
(update-camera)



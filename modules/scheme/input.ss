;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scratchpad
;; Functions available as part of the fluxus scratchpad.
;; Example:
;; EndSectionDoc

;; StartSectionDoc-fr
;; scratchpad
;; Fonctions disponibles à partir du scratchpad fluxus.
;; Exemple:
;; EndSectionDoc

#lang racket/base
(require "fluxus-modules.ss")
(provide 
 key-pressed
 keys-down
 key-special-pressed
 keys-special-down
 key-modifiers
 key-pressed-this-frame
 key-special-pressed-this-frame
 mouse-x
 mouse-y
 mouse-button
 mouse-over
 register-down
 register-up
 clear-down
 update-input
 mouse-wheel)

(define keys '())
(define keys-this-frame '())
(define special-keys '())
(define special-keys-this-frame '())
(define mouse (vector 0 0))
(define mouse-buttons (vector #f #f #f))
(define mouse-wheel-v 0)
(define key-mods '())

; utils funcs for using lists as sets
(define (set-remove a l)
  (if (null? l)
      '()
      (if (eq? (car l) a)
          (set-remove a (cdr l))
          (cons (car l) (set-remove a (cdr l))))))		  

(define (set-add a l)
  (if (not (memq a l))
      (cons a l)
      l))			  

(define (set-contains a l)
  (if (not (memq a l))
      #f
      #t))		  

(define (clear-down)
  (set! keys '()))

(define (update-input)
	(set! keys-this-frame '())
	(set! special-keys-this-frame '())
	(set! mouse-wheel-v 0))

(define (register-down key button special state x y mod)
  (when (not (or (number? key) (eq? key -1))) ; ordinary keypress
    (set! keys (set-add key keys))
	(set! keys-this-frame (set-add key keys-this-frame)))
  (when (not (= special -1)) ; special keypress
    (set! special-keys (set-add special special-keys))
	(set! special-keys-this-frame (set-add special special-keys-this-frame)))
  (set! key-mods ; key modifiers
	  (for/list ([bitmask (list 1 2 4)]
				 [bitsym '(shift ctrl alt)]
				 #:when (> (bitwise-and mod bitmask) 0))
			bitsym))
  (cond ; mouse
    ((and (eq? key 0) (eq? special -1)) 
	 (when (eq? button 3) (set! mouse-wheel-v 1))
	 (when (eq? button 4) (set! mouse-wheel-v -1))
     (when (and (eq? state 0)
				(< button (vector-length mouse-buttons)))
	   (vector-set! mouse-buttons button #t))
     (when (and (eq? state 1)
				(< button (vector-length mouse-buttons)))
	   (vector-set! mouse-buttons button #f))
     (vector-set! mouse 0 x)
     (vector-set! mouse 1 y))))

(define (register-up key button special state x y mod)
  (when (not (eq? key -1))
    (set! keys (set-remove key keys)))
  (when (not (eq? special -1))
    (set! special-keys (set-remove special special-keys))))

;; StartFunctionDoc-en
;; key-pressed key-string
;; Returns: boolean 
;; Description:
;; Returns true if the specified key is currently pressed down.
;; Example:
;; (when (key-pressed "q") (display "q pressed!"))
;; EndFunctionDoc	

;; StartFunctionDoc-pt
;; key-pressed string-tecla
;; Retorna: booleano
;; Descrição:
;; Retorna verdadeiro se a tecla especificada está apertada agora.
;; Exemplo:
;; (when (key-pressed "q") (display "q pressed!"))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; key-pressed key-chaine-de-caractères
;; Retour: booléen
;; Description:
;; Retourne vrai si la touche spécifiée est actuellement appuyée.
;; Exemple:
;; (when (key-pressed "q") (display "appui sur q!"))
;; EndFunctionDoc	

(define (key-pressed s)
  (set-contains (car (string->list s)) keys))

;; StartFunctionDoc-en
;; keys-down
;; Returns: keys-list 
;; Description:
;; Returns a list of keys pressed down
;; Example:
;; (display (keys-down))(newline)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; keys-down
;; Retorna: lista-teclas
;; Descrição:
;; Retorna uma lista das teclas pressionadas.
;; Exemplo:
;; (display (keys-down))(newline)
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; keys-down
;; Retour: keys-liste
;; Description:
;; Retourne la liste des touches appuyées
;; Exemple:
;; (display (keys-down))(newline)
;; EndFunctionDoc

(define (keys-down)
  keys)

;; StartFunctionDoc-en
;; key-special-pressed key-number
;; Returns: boolean 
;; Description:
;; Returns true if the specified special key is currently pressed down. Special
;; keys are ones which do not map to ascii values. The easiest way of finding
;; what they are is to print out the result of key-special-pressed while holding
;; down the key you are after.
;; Example: 
;; (when (key-special-pressed 100) (display "left cursor pressed"))
;; (when (key-special-pressed 102) (display "right cursor pressed"))
;; (when (key-special-pressed 101) (display "up cursor pressed"))
;; (when (key-special-pressed 103) (display "down cursor pressed"))
;; EndFunctionDoc	

;; StartFunctionDoc-pt
;; key-special-pressed número-tecla
;; Retorna: booleano
;; Descrição:
;; Retorna verdadeiro se a tecla especial especificada foi
;; pressionada. Teclas especiais são aquelas que não mapeiam para
;; valores ascii. A forma mais fácil de descubrir o que elas são é
;; imprimir o valor do resultado de key-special-pressed enquanto
;; segurando a tecla que você está procurando.
;; Exemplo:
;; (when (key-special-pressed 100) (display "left cursor pressed"))
;; (when (key-special-pressed 102) (display "right cursor pressed"))
;; (when (key-special-pressed 101) (display "up cursor pressed"))
;; (when (key-special-pressed 103) (display "down cursor pressed"))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; key-special-pressed key-nombre
;; Retour: booléen
;; Description:
;; Retourne vrai si la touche spéciale spécifiée est actuellement appuyée.
;; Les touches spéciales sont celles ne correspondant pas aux valeurs ascii.
;; La facon la plus facile de les trouver est d'afficher le résultat de key-special-pressed
;; en appuyant la touche désirée.
;; Exemple: 
;; (when (key-special-pressed 100) (display "appui curseur gauche"))
;; (when (key-special-pressed 102) (display "appui curseur droit"))
;; (when (key-special-pressed 101) (display "appui curseur haut"))
;; (when (key-special-pressed 103) (display "appui curseur bas"))
;; EndFunctionDoc	

(define (key-special-pressed k)
  (set-contains k special-keys))

;; StartFunctionDoc-en
;; keys-special-down
;; Returns: keys-list 
;; Description:
;; Returns a list of special keys pressed down
;; Example:
;; (display (keys-special-down))
;; EndFunctionDoc	

;; StartFunctionDoc-pt
;; keys-special-down
;; Retorna: lista-teclas
;; Descrição:
;; Retorna uma lista de teclas especiais pressionadas.
;; Exemplo:
;; (display (keys-special-down))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; keys-special-down
;; Retour: keys-liste
;; Description:
;; Retourne la liste des touches spéciales appuyées.
;; Exemple:
;; (display (keys-special-down))
;; EndFunctionDoc

(define (keys-special-down)
  special-keys)

;; StartFunctionDoc-en
;; key-modifiers
;; Returns: modifiers-list
;; Description:
;; Returns a list of key modifiers symbols consisting
;; 'shift, 'ctrl and 'alt.
;; Example:
;; (display (key-modifiers))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; key-modifiers
;; Retour: modifiers-liste
;; Description:
;; Retourne la liste des touches modificatrices sous formes
;; de symboles soit 'shift, 'ctrl and 'alt.
;; Exemple:
;; (display (key-modifiers))
;; EndFunctionDoc

(define (key-modifiers)
  key-mods)

;; StartFunctionDoc-en
;; key-pressed-this-frame key-string
;; Returns: boolean 
;; Description:
;; Returns true if the specified key was first pressed down this frame.
;; Example:
;; (when (key-pressed-this-frame "q") (display "q pressed!"))
;; EndFunctionDoc	

;; StartFunctionDoc-pt
;; key-pressed-this-frame string-tecla
;; Retorna: booleano
;; Descrição:
;; Retorna verdadeiro se a tecla especificada foi pressionada neste quadro.
;; Exemplo:
;; (when (key-pressed-this-frame "q") (display "q pressed!"))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; key-pressed-this-frame key-chaine-de-caractères
;; Retour: booléen
;; Description:
;; Retourne vrai si la touche spécifiée était la première appuyée
;; lors de cette image.
;; Exemple:
;; (when (key-pressed-this-frame "q") (display "appui sur q!"))
;; EndFunctionDoc	

(define (key-pressed-this-frame s)
  (set-contains (car (string->list s)) keys-this-frame))

;; StartFunctionDoc-en
;; key-special-pressed-this-frame key-number
;; Returns: boolean 
;; Description:
;; Returns true if the specified special key was first pressed down this frame.
;; Example:
;; (when (key-special-pressed 102) (display "right cursor pressed"))
;; EndFunctionDoc	

;; StartFunctionDoc-pt
;; key-special-pressed-this-frame número-tecla
;; Retorna: booleano
;; Descrição:
;; Retorna verdadeiro se a tecla especial foi pressionada neste quadro.
;; Exemplo:
;; (when (key-special-pressed 102) (display "right cursor pressed"))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; key-special-pressed-this-frame key-nombre
;; Retour: booléen
;; Description:
;; Retourne vrai si la touche spéciale spécifiée était la première appuyée
;; lors de cette image.
;; Exemple:
;; (when (key-special-pressed-this-frame 102) (display "appui curseur droit!"))
;; EndFunctionDoc	

(define (key-special-pressed-this-frame s)
  (set-contains s special-keys-this-frame))
  
;; StartFunctionDoc-en
;; mouse-x
;; Returns: coord-number 
;; Description:
;; Returns the x position of the mouse
;; Example:
;; (display (mouse-x))
;; EndFunctionDoc	

;; StartFunctionDoc-pt
;; mouse-x
;; Retorna: número-coord
;; Descrição:
;; Retorna a posição no eixo x do mouse.
;; Exemplo:
;; (display (mouse-x))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; mouse-x
;; Retour: coord-nombre
;; Description:
;; Retourne la position x de la souris
;; Exemple:
;; (display (mouse-x))
;; EndFunctionDoc	

(define (mouse-x)
  (vector-ref mouse 0))

;; StartFunctionDoc-en
;; mouse-y
;; Returns: coord-number 
;; Description:
;; Returns the y position of the mouse
;; Example:
;; (display (mouse-y))
;; EndFunctionDoc	

;; StartFunctionDoc-pt
;; mouse-y
;; Retorna: número-coord
;; Descrição:
;; Retorna a posição y do mouse
;; Exemplo:
;; (display (mouse-y))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; mouse-y
;; Retour: coord-nombre
;; Description:
;; Retourne la position y de la souris
;; Exemple:
;; (display (mouse-y))
;; EndFunctionDoc	

(define (mouse-y)
  (vector-ref mouse 1))

;; StartFunctionDoc-en
;; mouse-button button-number
;; Returns: boolean
;; Description:
;; Returns true if the specifed mouse button is pressed. Button numbers start
;; counting from 1 with the left mouse button.
;; Example:
;; (display (mouse-button 1))
;; EndFunctionDoc	

;; StartFunctionDoc-pt
;; mouse-button número-botão
;; Retorna: booleano
;; Descrição:
;; Retorna verdadeiro se o botão do mouse especificado foi
;; pressionado.
;; Exemplo:
;; (display (mouse-button 1))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; mouse-button bouton-nombre
;; Retour: booléen
;; Description:
;; Retourne vrai si le bouton de souris spécifié est appuyé. Les numéros de bouton
;; démarre le compte à 1 avec le bouton gauche de la souris.
;; Exemple:
;; (display (mouse-button 1))
;; EndFunctionDoc	

(define (mouse-button n)
  (and (<= 1 n (vector-length mouse-buttons))
	   (vector-ref mouse-buttons (- n 1))))
  
;; StartFunctionDoc-en
;; mouse-wheel
;; Returns: boolean
;; Description:
;; Returns 1 if the mouse wheel was moved in one direction in the last frame
;; or -1 if it was turned the other way, otherwise returns 0.
;; Example:
;; (display (mouse-wheel))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; mouse-wheel
;; Retorna: booleano
;; Descrição:
;; Retorna 1 se a rodinha do mouse foi movida em uma direção no último
;; quadro ou -1 se na direção contrária, ou então retorna 0.
;; Exemplo:
;; (display (mouse-wheel))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; mouse-wheel
;; Retour: booléen
;; Description:
;; Retourne 1 si la molette de la souris a bougée dans une direction
;; durant la dernière image, ou -1 si elle a tournée dans l'autre sens.
;; Autrement, 0.
;; Exemple:
;; (display (mouse-wheel))
;; EndFunctionDoc

(define (mouse-wheel)
	mouse-wheel-v)

;; StartFunctionDoc-en
;; mouse-over
;; Returns: primitiveid-number
;; Description:
;; Returns the object the mouse is currently over.
;; Example:
;; (grab (mouse-over))
;; (colour (vector 1 0 0)) ; paints objects the mouse is over red
;; (ungrab)
;; EndFunctionDoc	

;; StartFunctionDoc-pt
;; mouse-over
;; Retorna: número-primitivaid
;; Descrição:
;; Retorna o objeto que o mouse está em cima no momento.
;; Exemplo:
;; (grab (mouse-over))
;; (colour (vector 1 0 0)) ; paints objects the mouse is over red
;; (ungrab)
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; mouse-over
;; Retour: primitiveid-nombre
;; Description:
;; Retourne l'objet actuellement sous le curseur de la souris.
;; Exemple:
;; (grab (mouse-over))
;; (colour (vector 1 0 0)) ; paint l'objet sous la souris en rouge
;; (ungrab)
;; EndFunctionDoc	

(define (mouse-over)
  (select (vector-ref mouse 0) (vector-ref mouse 1) 3))



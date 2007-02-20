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
;; ScratchPadInput
;; Input functions availible in the fluxus scratchpad.
;; Example:
;; EndSectionDoc	

(module scratchpad-input mzscheme
	(require fluxus-engine)
	(provide 
		key-pressed
		keys-down
		key-special-pressed
		keys-special-down
		mouse-x
		mouse-y
		mouse-button
		mouse-over
		register-down
		register-up)

(define keys '())
(define special-keys '())
(define mouse (vector 0 0 #f))
 
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
		  
(define (register-down key button special state x y mod)
	(if (not (eq? key -1))
		(set! keys (set-add key keys)))
	(if (not (eq? special -1))
		(set! special-keys (set-add special special-keys)))
	(cond 
		((and (eq? key 0) (eq? special -1))
			(if (zero? state)
				(vector-set! mouse 2 (+ button 1))
				(vector-set! mouse 2 0))
			(vector-set! mouse 0 x)
			(vector-set! mouse 1 y))))

(define (register-up key button special state x y mod)
	(if (not (eq? key -1))
		(set! keys (set-remove key keys)))
	(if (not (eq? special -1))
		(set! special-keys (set-remove special special-keys))))

;; StartFunctionDoc-en
;; key-pressed key-string
;; Returns: boolean 
;; Description:
;; Returns true if the specified key is currently pressed down.
;; Example:
;; (if (key-pressed "q") (display "q pressed!))
;; EndFunctionDoc	

(define (key-pressed s)
	(set-contains (car (string->list s)) keys))

;; StartFunctionDoc-en
;; keys-down
;; Returns: keys-list 
;; Description:
;; Returns a list of keys pressed down
;; Example:
;; (display (keys-down))
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
;; (if (key-special-pressed 100) (display "left cursor pressed"))
;; (if (key-special-pressed 102) (display "right cursor pressed"))
;; (if (key-special-pressed 101) (display "up cursor pressed"))
;; (if (key-special-pressed 103) (display "down cursor pressed"))
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

(define (keys-special-down)
	special-keys)

;; StartFunctionDoc-en
;; mouse-x
;; Returns: coord-number 
;; Description:
;; Returns the x position of the mouse
;; Example:
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
	
(define (mouse-y)
	(vector-ref mouse 1))
	
;; StartFunctionDoc-en
;; mouse-button
;; Returns: boolean
;; Description:
;; Returns true if the specifed mouse button is pressed
;; Example:
;; (display (mouse-button 1))
;; EndFunctionDoc	

(define (mouse-button n)
	(eq? n (vector-ref mouse 2)))
	
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

(define (mouse-over)
	(select (vector-ref mouse 0) (vector-ref mouse 1) 3))
		
)

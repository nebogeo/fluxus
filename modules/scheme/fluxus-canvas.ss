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

; this script plugs the callbacks from the fluxus
; application into the fluxus engine module

;; StartSectionDoc-en
;; Canvas
;; The fluxus canvas is the main application and editor. 
;; Example:
;; EndSectionDoc 
		   
(module fluxus-canvas mzscheme
	(require fluxus-engine)
	(require fluxus-audio)
	(require (lib "fluxus-input.ss" "fluxus-0.12"))
	(require (lib "fluxus-camera.ss" "fluxus-0.12"))
	(require (only (lib "13.ss" "srfi") string-pad))
	(provide 
		fluxus-reshape-callback 
		fluxus-input-callback 
		fluxus-input-release-callback
		fluxus-frame-callback
		set-user-callback!
		every-frame
		clear
		start-framedump
		end-framedump)
		
;-------------------------------------------------
; every frame stuff 

(define user-callback '())

(define (set-user-callback! s)
	(set! user-callback s))
	
;; StartFunctionDoc-en
;; every-frame callback-function
;; Returns: void
;; Description:
;; Sets a function to be called every time the render is about to draw a new frame.
;; Example:
;; (define count 0)
;;
;; (define (myfunc)
;;     (display count)(display " frames have been rendered!")
;;     (newline)
;;     (set! count (+ count 1)))
;;
;; (every-frame (myfunc)) 
;; EndFunctionDoc	

; define the every-frame syntax
(define-syntax every-frame
  (syntax-rules ()
    ((every-frame expr)
     (set-user-callback! (lambda () expr)))))
	 
;; StartFunctionDoc-en
;; clear
;; Returns: void
;; Description:
;; Clears out the renderer of all objects and lights. Clears the physics system
;; and resets the every-frame callback. Generally a Good Thing to put this at the
;; beginning of scripts to make sure everything is cleared out each time you execute.
;; Example:
;; (clear) ; without this we would accumulate a new cube every time F5 was pressed
;; (build-cube) 
;; EndFunctionDoc	

(define (clear)
	(set! user-callback '())
	(clear-engine))
	
(define width 0)
(define height 0)

(define framedump-frame -1)
(define framedump-filename "")
(define framedump-type "")

;; StartFunctionDoc-en
;; start-framedump name-string type-string
;; Returns: void
;; Description:
;; Starts saving frames to disk. Type can be one of "tif", "jpg" or "ppm". 
;; Filenames are built with the frame number added, padded to 5 zeros.
;; Example:
;; (start-framedump "frame" "jpg") 
;; EndFunctionDoc	

(define (start-framedump filename type)
	(set! framedump-frame 0)
	(set! framedump-filename filename)
	(set! framedump-type type))
	
;; StartFunctionDoc-en
;; end-framedump 
;; Returns: void
;; Description:
;; Stops saving frames to disk. 
;; Example:
;; (end-framedump) 
;; EndFunctionDoc	

(define (end-framedump)
	(set! framedump-frame -1))
	
(define (framedump-update)
	(cond 
  		((>= framedump-frame 0)
			(let ((filename (string-append framedump-filename 
	 				(string-pad (number->string framedump-frame) 5 #\0) 
					"." framedump-type)))
			(display "saving frame: ")(display filename)(newline)
  	 		(framedump filename)
			(set! framedump-frame (+ framedump-frame 1))))))


;-------------------------------------------------
; callbacks - these are called directly from the
; fluxus application

; reshape function

(define (fluxus-reshape-callback x y)
  (set! width x)
  (set! height y)
  (reshape x y))

; input functions

(define (fluxus-input-callback key button special state x y mod)
	(register-down key button special state x y mod)
	(input-camera key button special state x y mod width height))

(define (fluxus-input-release-callback key button special state x y mod)
	(register-up key button special state x y mod))

; the main callback every frame

(define (fluxus-frame-callback) 
  (set-camera (get-camera-transform))
  (framedump-update)
  (begin-scene)
  (if (not (null? user-callback))
      (user-callback))
  (end-scene)
  (tick-physics)
  (update-audio))
  
)

;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

; this is the startup script for the fluxus scratchpad
; this script loads all the modules and sets things up so the 
; fluxus application works without having to worry about setup

; need to set some global stuff up, I know it's wrong, looking for a way around it.
; (how can we load the extensions before requiring the modules they contain)

; NOTE:
; Uncheck Language/Choose Language/Show Details/"Populate "compiled" directories for faster loading"
; otherwise an error message is displayed complaining about "make-directory: cannot make directory"


#lang racket/base

(require racket/class
         racket/gui/base
         ;mred/mred
         "fluxus.ss"
         (prefix-in gl- sgl/sgl))

(provide 
 (all-from-out "fluxus.ss"))

(define fluxus-collects-location (path->string (car (cdr (current-library-collection-paths)))))
(define fluxus-version "0.18")

(define fluxus-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    
    (define/override (on-paint)
      (with-gl-context
       (lambda ()           
         (fluxus-frame-callback)
         
         ; swap the buffers 
         (swap-gl-buffers)
         (super on-paint))))
    
    ;; route the events from mred into fluxus - this mostly consists of making
    ;; mred behave like glut with it's input formats (for the moment)
    (define/override (on-size width height)
      (with-gl-context
       (lambda () 
         (fluxus-reshape-callback width height))))
    
    ; mouse
    (define/override (on-event event)
      (let* ((type (send event get-event-type))
             (button (cond
                       ((or (send event get-left-down)
                            (eq? type 'left-up)
                            (eq? type 'left-down))
                        0)
                       ((or (send event get-middle-down)
                            (eq? type 'middle-down)
                            (eq? type 'middle-up))
                        1)
                       ((or (send event get-right-down)
                            (eq? type 'right-down)
                            (eq? type 'right-up))
                        2)
                       (else -1)))
             (state (cond 
                      ((send event button-down? 'any) 0)
                      (else 1))))
        
        (when (send event button-changed? 'any)
          (fluxus-input-callback 0 button -1 state (send event get-x) (send event get-y) 0))          
        (when (send event dragging?)
          (fluxus-input-callback 0 -1 -1 -1 (send event get-x) (send event get-y) 0))))
    
    ; keyboard
    (define/override (on-char event)
      (cond 
        ((equal? 'release (send event get-key-code))
         (clear-down)) ; todo: how to get key code from release?
        (else
         (fluxus-input-callback (send event get-key-code) 0 0 0 0 0 0))))
    
    (define (fluxus-canvas-new)  
      (super-instantiate () (style '(gl)))
      (with-gl-context
       (lambda () 
         (clear-texture-cache)      
         (fluxus-init))))
    
    (fluxus-canvas-new)))

(define frame (instantiate frame% ("drflux")))
(define fluxus-canvas (instantiate fluxus-canvas% (frame) (min-width 720) (min-height 576)))

(define (loop) 
  (send fluxus-canvas on-paint)
  (loop))

(define (init-me)
  ; make and show the window and canvas 
  (fluxus-reshape-callback 720 576)
  (send frame set-label (string-append "drflux " fluxus-version))
  (send frame show #t)
  (thread loop))

(init-me)



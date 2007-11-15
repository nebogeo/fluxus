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

; this is the startup script for the fluxus scratchpad
; this script loads all the modules and sets things up so the 
; fluxus application works without having to worry about setup

; need to set some global stuff up, I know it's wrong, looking for a way around it.
; (how can we load the extensions before requiring the modules they contain)

(module drflux mzscheme

  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "scratchpad.ss" "fluxus-0.14")
           (lib "gl.ss" "sgl")
           (prefix gl- (lib "sgl.ss" "sgl"))
           "fluxus-engine.ss"
           "fluxus-audio.ss")
  
  (provide 
  	(all-from (lib "scratchpad.ss" "fluxus-0.14")))
  
  (define fluxus-collects-location (path->string (car (cdr (current-library-collection-paths)))))
  (define fluxus-version "0.14")
  
  (define fluxus-canvas%
    (class* canvas% ()
      (inherit with-gl-context swap-gl-buffers)
      
      (define/override (on-paint)
        (with-gl-context
         (lambda ()           
           ; don't *need* to call this every frame, but 
           ; it doesn't hurt - just need to call it inside
           ; the gl context
           (fluxus-init)
           
           (fluxus-frame-callback)
           
           ; swap the buffers 
           (swap-gl-buffers)
           (super on-paint))))
      
      ;; route the events from mred into fluxus
      (define/override (on-size width height)
        (with-gl-context
         (lambda () 0)))
      
      (define/override (on-char event)
        (if (equal? 'release (send event get-key-code))
            (fluxus-input-release-callback (send event get-key-code) 0 0 0 0 0 0))
            (fluxus-input-callback (send event get-key-code) 0 0 0 0 0 0))
      
      (define (fluxus-canvas-new)
        (clear-texture-cache)
        (super-instantiate () (style '(gl))))
      
      (fluxus-canvas-new)))
    
  (define frame (instantiate frame% ("drflux")))
  (define fluxus-canvas (instantiate fluxus-canvas% (frame) (min-width 720) (min-height 576)))

  (define (loop) 
	(send fluxus-canvas on-paint)
	(loop))
  
  (define (init-me)
    ; make and show the window and canvas 
    (reshape 720 576)
    (send frame show #t)
    (thread loop))
  
  (init-me)
  
  )

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
;; scratchpad
;; The scratchpad is the fluxus editor and gl window. 
;; Example:
;; EndSectionDoc 

;; StartSectionDoc-pt
;; scratchpad
;; O scratchpad é o editor fluxus e a janela gl.
;; Exemplo:
;; EndSectionDoc

(module scratchpad mzscheme
  (require "fluxus-engine.ss")
  (require "fluxus-audio.ss")
  (require "fluxus-osc.ss")
  ; todo: get rid of burnt in versions
  (require (lib "scratchpad-input.ss" "fluxus-0.15"))
  (require (lib "scratchpad-help.ss" "fluxus-0.15"))
  (require (lib "scratchpad-camera.ss" "fluxus-0.15"))
  (require (lib "fluxus-building-blocks.ss" "fluxus-0.15"))
  (require (lib "fluxus-collada-import.ss" "fluxus-0.15"))
  (require (lib "fluxus-obj-import.ss" "fluxus-0.15"))
  (require (lib "fluxus-obj-export.ss" "fluxus-0.15"))
  (require (only (lib "13.ss" "srfi") string-pad))
  (provide 
   (all-from "fluxus-engine.ss")
   (all-from "fluxus-audio.ss")
   (all-from "fluxus-osc.ss")
   (all-from (lib "scratchpad-input.ss" "fluxus-0.15"))
   (all-from (lib "scratchpad-help.ss" "fluxus-0.15"))
   (all-from (lib "scratchpad-camera.ss" "fluxus-0.15"))
   (all-from (lib "fluxus-building-blocks.ss" "fluxus-0.15"))
   (all-from (lib "fluxus-collada-import.ss" "fluxus-0.15"))
   (all-from (lib "fluxus-obj-import.ss" "fluxus-0.15"))
   (all-from (lib "fluxus-obj-export.ss" "fluxus-0.15"))
   fluxus-reshape-callback 
   fluxus-input-callback 
   fluxus-input-release-callback
   fluxus-frame-callback
   override-frame-callback
   set-user-callback!
   every-frame
   clear
   start-framedump
   end-framedump
   get-eye-separation
   set-eye-separation
   set-physics-debug
   )
  
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

  ;; StartFunctionDoc-pt
  ;; every-frame função-callback
  ;; Retorna: void
  ;; Descrição:
  ;; Ajusta uma função pra ser chamada todo o tempo em que o render
  ;; está para desenhar um novo quadro.
  ;; Exemplo:
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
  
  ;; StartFunctionDoc-pt
  ;; clear
  ;; Retorna: void
  ;; Descrição:
  ;; Limpa o renderizador de todos os objetos e luzes. Limpa o sistema
  ;; de física e re-inicializa a chamada de volta em
  ;; every-frame. Geralmente uma boa coisa a fazer é colocar isto no
  ;; ínicio dos scripts pra ter certeza que tudo esta limpo cada vez
  ;; que você chamar a execução.
  ;; Exemplo:
  ;; (clear) ; sem isso a gente ia acumular um novo cubo toda vez que F5 fosse pressionado
  ;; (build-cube) 
  ;; EndFunctionDoc

  (define (clear)
    (set! user-callback '())
    (clear-engine)
    (unlock-camera))
  
  (define width 0)
  (define height 0)
  (define physics-debug #f)
    
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
  
  ;; StartFunctionDoc-pt
  ;; start-framedump string-nome string-tipo
  ;; Retorna: void
  ;; Descrição:
  ;; Inicia a gravação de quadros no disco. Tipo pode ser um dos:
  ;; "tif", "jpg" ou "ppm". Nomes dos arquivos são construidos junto
  ;; com o número do quadro adicionado, prefixado em 5 zeros.
  ;; Exemplo:
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

  ;; StartFunctionDoc-pt
  ;; end-framedump
  ;; Retorna: void
  ;; Descrição:
  ;; Para a gravação de quadros para o disco.
  ;; Exemplo:
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

  ;; StartFunctionDoc-en
  ;; set-physics-debug boolean
  ;; Returns: void
  ;; Description:
  ;; Call with #t to turn on debug rendering for the physics.
  ;; Example:
  ;; (set-physics-debug #t) 
  ;; EndFunctionDoc    

  ;; StartFunctionDoc-pt
  ;; set-physics-debug boolean
  ;; Retorna: void
  ;; Descrição:
  ;; Call with #t to turn on debug rendering for the physics.
  ;; Exemplo:
  ;; (set-physics-debug #t)
  ;; EndFunctionDoc
  
  (define (set-physics-debug s)
  	(set! physics-debug s))
  
  ;-------------------------------------------------
  ; stereo mode
  
  (define eye-separation 0.3)
  (define (get-eye-separation) eye-separation)
  (define (set-eye-separation val) (set! eye-separation val))    
  
  (define (stereo-render)
    (let ((stereo-mode (get-stereo-mode))
          (half_sep (/ (get-eye-separation) 2))
          (left-eye-colour-mask #(#t #f #f #t))
          (right-eye-colour-mask #(#f #f #t #t)))
      (cond 
        ((eq? stereo-mode 'crystal-eyes)
         ; draw for left eye
         (draw-buffer 'back-left)
         (set-camera 
          (mmul 
           (mtranslate (vector (- half_sep) 0 0))
           (get-camera-transform)))
         (if (not (null? user-callback))
             (user-callback))
         (fluxus-render)
         
         ; draw for right eye
         (draw-buffer 'back-right)
         (set-camera 
          (mmul 
           (mtranslate (vector half_sep 0 0))
           (get-camera-transform)))
         (if (not (null? user-callback))
             (user-callback))
         (fluxus-render)
         
         ; reset for other drawing
         (draw-buffer 'back))
        
    ((eq? stereo-mode 'colour)
         ;left
         (set-colour-mask left-eye-colour-mask)
         (clear-frame 1)
         (set-camera 
          (mmul 
           (mtranslate (vector (- half_sep) 0 0))
           (get-camera-transform)
           ))
         (if (not (null? user-callback))
             (user-callback))
         (fluxus-render)
         
         ;right
         (set-colour-mask right-eye-colour-mask)
         (clear-frame 0)
         (set-camera 
          (mmul 
           (mtranslate (vector half_sep 0 0))
           (get-camera-transform)
           ))
         (if (not (null? user-callback))
             (user-callback))
         (fluxus-render)
         ;reset
         (set-colour-mask #(#t #t #t #t))))))
  
  ;-------------------------------------------------
  ; callback-override
  
  ;; StartFunctionDoc-en
  ;; callback-override callback-function
  ;; Returns: void
  ;; Description:
  ;; Allows you to override the frame callback, to control
  ;; the rendering loop of fluxus in a more detailed way.
  ;; Example:
  ;; (callback-override myfunc) 
  ;; EndFunctionDoc    

  ;; StartFunctionDoc-pt
  ;; callback-override função-callback
  ;; Retorna: void
  ;; Descrição:
  ;; Permite que você substitua a chamada de volta (callback) do quadro,
  ;; para controlar o loop de renderização do fluxus de uma forma mais detalhada.
  ;; Exemplo:
  ;; (callback-override myfunc)
  ;; EndFunctionDoc
  
  (define (override-frame-callback fn)
      (set! fluxus-frame-callback fn))
  
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
    (cond 
      ((eq? (get-stereo-mode) 'no-stereo)
            (draw-buffer 'back)
            (set-camera (get-camera-transform))
            (framedump-update)
            (if (not (null? user-callback))
				(with-state
    	           (user-callback)))
            (fluxus-render)
			(if physics-debug (render-physics))
            (tick-physics)
            (update-audio))
      (else
       (stereo-render)))
     (display (fluxus-error-log)))
  )

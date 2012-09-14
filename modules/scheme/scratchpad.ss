;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scratchpad
;; Functions available as part of the fluxus scratchpad.
;; Example:
;; EndSectionDoc 

;; StartSectionDoc-pt
;; scratchpad
;; O scratchpad é o editor fluxus e a janela gl.
;; Exemplo:
;; EndSectionDoc

#lang racket/base

(require "fluxus-modules.ss"
		 "input.ss"
		 "help.ss"
		 "camera.ss"
		 "building-blocks.ss"
		 "tasks.ss")
		 
(provide 
 fluxus-auto-indent
 set-auto-indent-tab
 set-camera-update
 fluxus-reshape-callback 
 fluxus-input-callback 
 fluxus-input-release-callback
 fluxus-frame-callback
 override-frame-callback
 every-frame
 clear
 start-framedump
 end-framedump
 get-eye-separation
 set-eye-separation
 set-physics-debug
 default-fluxus-frame-callback
 )

;-------------------------------------------------
; every frame stuff   

;; StartFunctionDoc-en
;; every-frame callback-function
;; Returns: void
;; Description:
;; Sets a function to be called every time the render is about to draw a new frame.
;; Example:
;; (define (myfunc)
;;     (colour (rndvec))
;;     (draw-torus))
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
;; (define (myfunc)
;;     (colour (rndvec))
;;     (draw-torus))
;;
;; (every-frame (myfunc)) 
;; EndFunctionDoc

; define the every-frame syntax
(define-syntax every-frame
  (syntax-rules ()
    ((every-frame expr)
	 (spawn-task (lambda () expr) 'every-frame-task))))

;---------------------------------------------------------------

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
  (rm-task 'every-frame-task)
  (clear-engine)
  (ffgl-clear-instances)
  (light-diffuse 0 (vector 1 1 1))
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
   
 (define (string-pad b)
   (substring (number->string (+ b 100000)) 1 6))

(define (framedump-update)
  (cond 
    ((>= framedump-frame 0)
     (let ((filename (string-append framedump-filename 
                                    (string-pad framedump-frame) 
                                    "." framedump-type)))
       ;(display "saving frame: ")(display filename)(newline)
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
       (do-render)
       
       ; draw for right eye
       (draw-buffer 'back-right)
       (set-camera 
        (mmul 
         (mtranslate (vector half_sep 0 0))
         (get-camera-transform)))
       (do-render)
       
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
       (do-render)
       
       ;right
       (set-colour-mask right-eye-colour-mask)
       (clear-frame 0)
       (set-camera 
        (mmul 
         (mtranslate (vector half_sep 0 0))
         (get-camera-transform)
         ))
       (do-render)
       ;reset
       (clear-frame 1)
       (set-colour-mask #(#t #t #t #t))))))

;-------------------------------------------------
; callback-override

;; StartFunctionDoc-en
;; override-frame-callback callback-function
;; Returns: void
;; Description:
;; Allows you to override the frame callback, to control
;; the rendering loop of fluxus in a more detailed way.
;; Example:
;; (override-frame-callback myfunc) 
;; (override-frame-callback default-fluxus-frame-callback) ; set it back again...
;; EndFunctionDoc    

;; StartFunctionDoc-pt
;; override-frame-callback função-callback
;; Retorna: void
;; Descrição:
;; Permite que você substitua a chamada de volta (callback) do quadro,
;; para controlar o loop de renderização do fluxus de uma forma mais detalhada.
;; Exemplo:
;; (override-frame-callback myfunc)
;; (override-frame-callback default-fluxus-frame-callback) ; set it back again...
;; EndFunctionDoc

(define (override-frame-callback fn)
  (set! fluxus-frame-callback fn))

;; StartFunctionDoc-en
;; set-auto-indent-tab size-number
;; Returns: void
;; Description:
;; Sets the tabs size for the prettification auto indent on ctrl-p. Defaults to 2.
;; Example:
;; (set-auto-indent-tab 2)
;; EndFunctionDoc 

(define fluxus-auto-tab-size 4)

(define (set-auto-indent-tab s)
	(set! fluxus-auto-tab-size s))
	
(define camera-update-a #t)

;; StartFunctionDoc-en
;; set-camera-update #t/#f
;; Returns: void
;; Description:
;; Turns off camera update - allowing you to use (set-camera) - otherwise it gets
;; written over by the mouse camera update. The reason for needing this is that 
;; (set-camera-transform) doesn't work with multiple cameras - need to fix.
;; Example:
;; (set-camera-update #f)
;; (set-camera-update #t)
;; EndFunctionDoc 

(define (set-camera-update s)
    (set! camera-update-a s))
		
(define (do-render)
	 (with-state (run-tasks))
     (fluxus-render))
	 
;-------------------------------------------------
; callbacks - these are called directly from the
; fluxus application

; used for pretty formatting

(define (fluxus-auto-indent text)
  (let ((out "")
        (d 0)
        (newline #f)
        (prewhite #f))
    (for ((i (in-range 0 (string-length text))))
         (let ((c (string-ref text i)))
           (cond 
             ((char=? c #\newline) 
              (set! newline #t)
              (set! out (string-append out (string #\newline) 
			  	(make-string (* d fluxus-auto-tab-size)#\ ))))
             (else
              (cond
                ((char=? c #\() (set! d (+ d 1)))
                ((char=? c #\)) (when (> d 0) (set! d (- d 1))))
                ((char=? c #\[) (set! d (+ d 1)))
                ((char=? c #\]) (when (> d 0) (set! d (- d 1))))
                ((char=? c #\{) (set! d (+ d 1)))
                ((char=? c #\}) (when (> d 0) (set! d (- d 1)))))
              (when (and newline (not (char-whitespace? c)))
                  (set! newline #f))
              (when (not newline) 
                (set! out (string-append out (string c))))))))
    out))

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

(define (default-fluxus-frame-callback) 
  (cond 
    ((eq? (get-stereo-mode) 'no-stereo)
     (draw-buffer 'back)
     (when camera-update-a (set-camera (get-camera-transform)))
     (framedump-update)
     (do-render)
     (when physics-debug (render-physics)))
    (else
     (stereo-render)))
  (tick-physics)
  (update-audio)
  (oa-update)
  (update-input)
  (display (fluxus-error-log)))

(define fluxus-frame-callback default-fluxus-frame-callback)

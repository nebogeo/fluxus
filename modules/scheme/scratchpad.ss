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
;; ScratchPad
;; The scratchpad is the fluxus editor and gl window. 
;; Example:
;; EndSectionDoc 

;; StartSectionDoc-pt
;; ScratchPad
;; O scratchpad é o editor fluxus e a janela gl.
;; Example:
;; EndSectionDoc

(module scratchpad mzscheme
  (require fluxus-engine)
  (require fluxus-audio)
  ; todo: get rid of burnt in versions
  (require (lib "scratchpad-input.ss" "fluxus-0.13"))
  (require (lib "scratchpad-camera.ss" "fluxus-0.13"))
  (require (only (lib "13.ss" "srfi") string-pad))
  (provide 
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
   init-help
   help)
  
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
  
  ;-------------------------------------------------
  ; online help system
      
  (define helpmap '())
  
  (define (insert-linebreaks src dst count i n)
    (if (>= i (string-length src))
        dst
        (if (and (> n count) (char=? (string-ref src i) #\space))
            (insert-linebreaks src 
                               (string-append dst (string (string-ref src i)) (string #\newline)) count (+ i 1) 0)
            (insert-linebreaks src 
                               (string-append dst (string (string-ref src i))) count (+ i 1) (+ n 1)))))
 
  (define (func-help funcname)  
    (define (inner-help l)
      (let ((ret (assoc funcname (list-ref (cadr (car l)) 2))))
        (cond
          (ret
           (display "Function")(newline)
           (display "(")(display (car ret))
           (let ((arguments (list-ref (list-ref ret 1) 0)))
             (cond 
               ((not (zero? (string-length arguments)))
                (display " ")
                (display arguments))))
           (display ")")(newline)(newline)
           (display "Returns ")
           (display (list-ref (list-ref ret 1) 1))(newline)(newline)
           (display "Description")(newline)
           (display (insert-linebreaks (list-ref (list-ref ret 1) 2) "" 50 0 0))
           (newline)(newline)
           (display "Example")(newline)
           (display (list-ref (list-ref ret 1) 3))
           (newline))
          (else
           (if (null? (cdr l))
               "Function not found"
               (inner-help (cdr l)))))))
    (cond 
      ((null? helpmap)
       (display "No helpmap exists...")(newline)
       (display "Try running \"makedocs.sh\" in the fluxus docs directory")(newline))
      (else
       (inner-help helpmap))))

  ; just print out the example for (preformatted) documentation which isn't
  ; really a function - need to do this in a more general way...!
  (define (func-help-example funcname)  
    (define (inner-help l)
      (let ((ret (assoc funcname (list-ref (cadr (car l)) 2))))
        (cond
          (ret
           (display (list-ref (list-ref ret 1) 3))
           (newline))
          (else
           (if (null? (cdr l))
               "Function not found"
               (inner-help (cdr l)))))))
    (cond 
      ((null? helpmap)
       (display "No helpmap exists...")(newline)
       (display "Try running \"makedocs.sh\" in the fluxus docs directory")(newline))
      (else
       (inner-help helpmap))))
  
  (define (init-help helpmapfile)
    (cond 
      ((file-exists? helpmapfile)
       (let ((file (open-input-file helpmapfile)))
         (set! helpmap (read file))
         (close-input-port file)))))

  ; (these aren't really functions, but the documentation printed when you
  ; type (help) for more general things - maybe I need to make the documentation 
  ; more general to cope better with bits of extra documentation like this)
  ; -greb- i agree here, not that it is bad as it is, what we need are
  ; ways to navigate trough functions definitions, perhaps we could
  ; make it more fancy using those "thought bubbles" and make it
  ; toggleable  to appear with a key like F1 when the cursor is over a
  ; function name? (just an idea)
  
  ;; StartFunctionDoc-en
  ;; tophelp
  ;; Returns: 
  ;; Description:
  ;; Example:
  ;; Fluxus documentation
  ;; --------------------
  ;; 
  ;; Fluxus is a realtime rendering engine for livecoding in Scheme.
  ;; 
  ;; The fluxus scratchpad has two modes of operation, the console
  ;; (you are using this now) which allows you to enter commands and
  ;; see the results immediately. The other mode is the editor which 
  ;; is more like a normal text editor - there are 9 workspaces,
  ;; (which allow you to edit more than one script at once) switch to 
  ;; them using ctrl-1 to ctrl-9 and switch back to the console with 
  ;; ctrl-0.
  ;;
  ;; More help topics:
  ;; (help "keys") for keyboard commands for controlling fluxus 
  ;; (help "console") for more help on the console 
  ;; (help "editor") for more help on the livecoding editor
  ;; (help "camera") for help on the camera controls 
  ;; (help "functionname") to find out more information about a function
  ;; (help "misc") for miscellaneous fluxus info 
  ;; EndFunctionDoc	
  
  ;; StartFunctiondoc-pt
  ;; tophelp
  ;; Retorna:
  ;; Descrição:
  ;; Exemplo:
  ;; Fluxus docs
  ;; -----------
  ;; Fluxus é uma máquina de renderização em tempo real para
  ;; livecoding em Scheme.
  ;; 
  ;; O scratchpad de Fluxus têm dois modos de operação, o console
  ;; (você está usando ele agora) que permite que você entre comandos
  ;; e veja o resultado imediatamente. O outro modo é o editor que se
  ;; parece mais com um editor de texto normal -  você tem 9 áreas de
  ;; trabalho( que permite que você edite mais de um script de uma
  ;; vez) vá para eles usando ctrl-1 até ctrl-9 e venha de volta para
  ;; o console através do atalho ctrl-0.                                   
  ;;
  ;; Mais tópicos de ajuda:
  ;; (help "keys") para comandos do teclado que controlam o fluxus.
  ;; (help "console") para mais ajuda sobre o console
  ;; (help "editor") para mais ajuda no editor de livecoding
  ;; (help "camera") para ajuda nos controles da câmera.
  ;; (help "functionname") para encontrar mais informação sobre uma função.
  ;; (help "misc") para informações miscelâneas sobre o fluxus
  ;; EndFunctionDoc

  ;; StartFunctionDoc-en
  ;; keys
  ;; Returns: 
  ;; Description:
  ;; Example:
  ;; Fluxus keys
  ;; -----------
  ;;
  ;; ctrl-f : Fullscreen mode.
  ;; ctrl-w : Windowed mode.
  ;; ctrl-h : Hide/show the editor.
  ;; ctrl-l : Load a new script (navigate with cursors and return).
  ;; ctrl-s : Save current script.
  ;; ctrl-d : Save as - current script (opens a filename dialog).
  ;; ctrl-1 to 9 : Switch to selected workspace.
  ;; ctrl-0 : Switch to the REPL.
  ;; F3 : Resets the scene camera.
  ;; F5 : Execute the selected text, or all if none is selected.
  ;; F9 : Randomise the text colour (aka the panic button)
  ;; F10-F11 : Make text thinner/thicker (these two are for use with
  ;; projectors to make the text more visible)
  ;; EndFunctionDoc
 
  ;; StartFunctionDoc-pt
  ;; keys
  ;; Retorna: 
  ;; Descrição:
  ;; Exemplo:
  ;; Teclas do Fluxus
  ;; ----------------
  ;; 
  ;; ctrl-f : modo Tela cheia.
  ;; ctrl-w : modo Janela.
  ;; ctrl-h : esconder/mostrar o editor.
  ;; ctrl-l : Carregar um novo script (navegue com os cursores e enter)
  ;; ctrl-s : Salve o script atual.
  ;; ctrl-d : Salvar como - o script atual (abre um caixa com nome)
  ;; ctrl-1 to 9 : Vai para a área de trabalho selecionada.
  ;; ctrl-0 : Vai para o REPL.
  ;; F3 : Reinicia a câmera.
  ;; F5 : Executa o texto selecionado, ou tudo se não tem nada selecionado.
  ;; F9 : Randomisa a cor do texto (aka botão panico)
  ;; F10-F11 ; Faz o texto mais fino/grosso (estes dois são para uso com
  ;; projetores para fazer com que o texto fique visivel)
  ;; EndFunctionDoc

  ;; StartFunctionDoc-en
  ;; console
  ;; Returns: 
  ;; Description:
  ;; Example:
  ;; Fluxus console (or REPL) 
  ;; ------------------------
  ;;
  ;; If you press ctrl and 0, instead of getting another script workspace, 
  ;; you will be presented with a Read Evaluate Print Loop interpreter, or 
  ;; repl for short. This is really just an interactive interpreter similar 
  ;; to the commandline, where you can enter scheme code for immediate 
  ;; evaluation. This code is evaluated in the same interpreter as the other 
  ;; scripts, so you can use the repl to debug or inspect global variables 
  ;; and functions they define. This window is also where error reporting is
  ;; printed, along with the terminal window you started fluxus from.
  ;; EndFunctionDoc	

  ;; StartFunctionDoc-pt
  ;; console
  ;; Retorna: 
  ;; Descrição:
  ;; Exemplo:
  ;; Console do Fluxus (ou REPL)
  ;; ---------------------------
  ;;
  ;; Se você pressionar ctrl e 0, ao invés de abrir outra área de
  ;; trabalho para scripts você vai ser apresentado a um interpretador
  ;; de Leitura Evaluação Impressão e Loop, ou repl por
  ;; diminutivo. Isto realmente é um interpretador interativo similar
  ;; a uma linha de comando, onde você pode entrar código scheme para
  ;; evaluação imediata. Este código é evaluado no mesmo interpretador
  ;; que os outros scripts, então você pode usar o repl para debug ou
  ;; inspecionar variáveis globais e funções que elas definem. Esta
  ;; janela é também onde todo os erros são impressos, junto com a
  ;; janela de terminal que você iniciou o fluxus.
  ;; EndFunctionDoc

  ;; StartFunctionDoc-en
  ;; editor
  ;; Returns: 
  ;; Description:
  ;; Example:
  ;; Fluxus editor 
  ;; -------------
  ;;
  ;; When using the fluxus scratchpad, the idea is that you only need the one 
  ;; window to build scripts, or play live. f5 is the key that runs the script 
  ;; when you are ready.  Selecting some text (using shift) and pressing f5 will 
  ;; execute the selected text only. This is handy for reevaluating functions 
  ;; without running the whole script each time.
  ;;
  ;; Workspaces
  ;; ----------
  ;;
  ;; The script editor allows you to edit 9 scripts simultaneously by using
  ;; workspaces. To switch workspaces, use ctrl+number key. Only one can be run 
  ;; at once though, hitting f5 will execute the currently active workspace
  ;; script. 
  ;; EndFunctionDoc	

  ;; StartFunctionDoc-pt
  ;; editor
  ;; Retorna: void
  ;; Descrição:
  ;; Exemplo:
  ;; Editor Fluxus
  ;; -------------
  ;;
  ;; Quando usar o scratchpad do fluxus, a idéia é que você só precise
  ;; da única tela para construir os scripts, ou tocar ao vivo, F5 é a
  ;; chave que roda o script quando vc estiver pronto. Selecionando
  ;; texto (usando shift) e pressionando F5 vai executar somente o
  ;; texto selecionado. Isto é uma mão na roda para re-evaluar funções
  ;; sem rodar o script inteiro toda vez.
  ;;
  ;; Áreas de trabalho
  ;; -----------------
  ;;
  ;; O editor de script permite que voce edite até 9 scripts
  ;; simultaneamente usando diferentes áreas de trabalho. Para trocas
  ;; de área, use ctrl+tecla de número. Somente um script pode rodar
  ;; de uma vez no entanto, apertando F5 vai executar o script da área
  ;; de trabalho ativa atualmente.
  ;; EndFunctionDoc

  ;; StartFunctionDoc-en
  ;; camera
  ;; Returns: 
  ;; Description:
  ;; Example:
  ;; Fluxus camera control
  ;; ---------------------
  ;;
  ;; The camera is controlled by moving the mouse and pressing mouse buttons.
  ;;
  ;; Left mouse button: Rotate
  ;; Middle mouse button: Move
  ;; Right mouse button: Zoom
  ;; EndFunctionDoc

  ;; StartFunctionDoc-pt
  ;; camera
  ;; Retorna: void
  ;; Descrição:
  ;; Exemplo:
  ;; Fluxus camera control
  ;; ---------------------
  ;; 
  ;; A câmera é controlada ao mover o mouse e pressionar os botões ao
  ;; mesmo tempo.
  ;;
  ;; Botão esquerdo do mouse: Rotaciona
  ;; Botão do meio do mouse: Movimenta
  ;; Botão direito do mouse: Afasta/Aproxima(zoom)
  ;; EndFunctionDoc

  ;; StartFunctionDoc-en
  ;; misc
  ;; Returns: 
  ;; Description:
  ;; Example:
  ;; Misc
  ;; ----
  ;;
  ;;
  ;; EndFunctionDoc

  ;; StartFunctionDoc-pt
  ;; misc
  ;; Retorna: void
  ;; Descrição:
  ;;
  ;; Exemplo:
  ;; Misc
  ;; ----
  ;;
  ;;
  ;; EndFunctionDoc

  ;; StartFunctionDoc-en
  ;; help function-string
  ;; Returns: void
  ;; Description:
  ;; Displays help information on a fluxus function. For running in the repl mainly.
  ;; Example:
  ;; (help "pop") 
  ;; EndFunctionDoc	

  ;; StartFunctionDoc-pt
  ;; help string-função
  ;; Retorna: void
  ;; Descrição:
  ;; Mostra informação de ajuda numa função do fluxus. Para rodar no
  ;; repl principalmente.
  ;; Exemplo:
  ;; (help "pop")
  ;; EndFunctionDoc
  
  (define (help . args)
    (if (null? args) ; help without an argument prints out some 
        (func-help-example "tophelp")   ; top level help information (defined above)
        (cond
          ((string=? (car args) "keys") (func-help-example (car args)))
          ((string=? (car args) "console") (func-help-example (car args)))
          ((string=? (car args) "editor") (func-help-example (car args)))
          ((string=? (car args) "camera") (func-help-example (car args)))
          ((string=? (car args) "misc") (func-help-example (car args)))
          (else
           (func-help (car args))))))
  
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
  ;; callback-override
  ;; Retorna: void
  ;; Descrição:
  ;; Permite que você substitua a chama de volta (callback) do quadro,
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
  	(identity) ; clear the last transform (I can't decide if this should 
	           ; be the standard behavour, so it's here for now)
    (cond 
      ((eq? (get-stereo-mode) 'no-stereo)
	        (draw-buffer 'back)
            (set-camera (get-camera-transform))
            (framedump-update)
            (if (not (null? user-callback))
               (user-callback))
            (fluxus-render)
            (tick-physics)
            (update-audio))
      (else
       (stereo-render)))
	 (display (fluxus-error-log)))
  )

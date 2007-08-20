;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fluxus scratchpad docs

;; StartSectionDoc-en
;; ScratchPadDocs
;; The scratchpad is the fluxus editor and gl window. 
;; Example:
;; EndSectionDoc 

;; StartSectionDoc-pt
;; ScratchPadDocs
;; O scratchpad é o editor fluxus e a janela gl.
;; Exemplo:
;; EndSectionDoc

; This is the documentation printed when you type (help) for more general things - 
; maybe I need to make the documentation more general to cope better with bits of 
; extra documentation like this)
; -greb- i agree here, not that it is bad as it is, what we need are
; ways to navigate trough functions definitions, perhaps we could
; make it more fancy using those "thought bubbles" and make it
; toggleable  to appear with a key like F1 when the cursor is over a
; function name? (just an idea)
; -dave- I've moved this stuff here...
; I like that idea a lot, but simple things first! ;)

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
;; (help "language") to browse functions
;; (help "misc") for miscellaneous fluxus info 
;; (help "name-of-any-function") to find out more information about a function
;; EndFunctionDoc    

;; StartFunctionDoc-pt
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
;; language
;; Returns: 
;; Description:
;; Example:
;; Language Docs
;; -------------
;; 
;; Fluxus is comprised of a set of functions which
;; extend Scheme for use in realtime computer graphics.
;; The functions are grouped into sections to make things
;; a bit easier.
;;
;; (help "sections") for a list of all sections
;; (help "sectionname") to find out more about a section
;; (help "functionname") to find out more about a function
;;
;; The idea is that you can find a function you are interested 
;; in by doing something like this:
;;
;; (help "sections")
;;    ... list of sections ...
;; (help "maths")
;;    ... description and list of maths functions ...
;; (help "vmul")
;;    ... details about the function with example ...
;;
;;
;; EndFunctionDoc

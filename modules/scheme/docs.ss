;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fluxus scratchpad docs

;; StartSectionDoc-en
;; high-level-scratchpad-docs
;; Some useful high level documentation lives here, this won't make much
;; sense if you are reading this in a different place from the help system
;; in the fluxus scratchpad app, but it might be useful anyway...
;; Example:
;; EndSectionDoc

;; StartSectionDoc-pt
;; high-level-scratchpad-docs
;; O scratchpad é o editor fluxus e a janela gl.
;; Exemplo:
;; EndSectionDoc

;; StartSectionDoc-fr
;; high-level-scratchpad-docs
;; Documentation utile de haut niveau.
;; Exemple:
;; EndSectionDoc

; This is the documentation printed when you type (help) for more general things -
; maybe I need to make the documentation more general to cope better with bits of
; extra documentation like this)
; -greb- i agree here, not that it is bad as it is, what we need are
; ways to navigate trough functions definitions, perhaps we could
; make it more fancy using those "thought bubbles" and make it
; toggleable  to appear with a key like F1 when the cursor is over a
; function name? (just an idea)
;
; -dave- I've moved this stuff here...
; I like that idea a lot (thought bubbles), but simple things first! ;)
; BTW, the text here is preformatted, and the font has got
; bigger, so the line breaks need tweaking (I was scared to do
; the portuguese as my editor can't handle utf-8 (ironically :)
;
;; StartFunctionDoc-en
;; tophelp
;; Returns:
;; Description:
;; Example:
;; Fluxus documentation
;; --------------------
;; "act of a flowing; a continuous moving on or passing by, as of a
;; flowing stream; a continuous succession of changes"
;;
;; Fluxus is a realtime rendering engine for livecoding in Scheme.
;; For more detailed docs, see: fluxus/docs/fluxus-documentation.txt
;;
;; The fluxus scratchpad has two modes of operation, the console
;; (you are using this now) which allows you to enter commands and
;; see the results immediately. The other mode is the editor which
;; is more like a normal text editor - there are 9 workspaces,
;; (which allow you to edit more than one script at once) switch to
;; them using ctrl-1 to ctrl-9 and switch back to the console with
;; ctrl-0.
;;
;; To copy/paste examples, hit the right cursor until you move behind
;; the prompt, navigate to the example, use shift to select it,
;; press ctrl-c to copy, then ctrl-0 and ctrl-v to paste into a
;; text buffer.
;;
;; More help topics:
;; (help "keys") for keyboard commands for controlling fluxus
;; (help "console") for more help on the console
;; (help "editor") for more help on the livecoding editor
;; (help "camera") for help on the camera controls
;; (help "language") for more info on the fluxus commands
;; (help "misc") for miscellaneous fluxus info
;; (help "toplap") for the toplap manefesto
;; (help "authors") who made this?
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; tophelp
;; Retorna:
;; Descrição:
;; Exemplo:
;; Fluxus docs
;; -----------
;; "act of a flowing; a continuous moving on or passing by, as of a
;; flowing stream; a continuous succession of changes"
;;
;; "ato de fluir; um movimento contínuo ou um passar, como a corrente
;; do rio; uma sucessão contínua de mudanças"
;;
;; Fluxus é uma máquina de renderização em tempo real para
;; livecoding em Scheme.
;; Você é encorajado a viajar por estes textos enquanto performar
;; códigos ao vivo...
;;
;; O scratchpad de Fluxus têm dois modos de operação, o console
;; (você está usando ele agora) que permite que você entre comandos
;; e veja o resultado imediatamente. O outro modo é o editor que se
;; parece mais com um editor de texto normal -  você tem 9 áreas de
;; trabalho( que permite que você edite mais de um script de uma
;; vez) vá para eles usando ctrl-1 até ctrl-9 e venha de volta para
;; o console com ctrl-0.
;;
;; Mais tópicos de ajuda:
;; (help "keys") para comandos do teclado que controlam o fluxus.
;; (help "console") para mais ajuda sobre o console
;; (help "editor") para mais ajuda no editor de livecoding
;; (help "camera") para ajuda nos controles da câmera.
;; (help "linguagem") informação sobre os comandos no fluxus.
;; (help "misc") para informações miscelâneas sobre o fluxus.
;; (help "toplap") para o manifesto toplap.
;; (help "autores") quem fez isso?
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; tophelp
;; Retour:
;; Description:
;; Exemple:
;; Fluxus documentation
;; --------------------
;;
;; acte d'un fluide; un avancement continuel ou un passage,
;; comme celui d'un ruisseau qui coule; une succession continue de changements 
;;
;; Fluxus est un moteur de rendu temps-réel pour le livecoding en Scheme.
;; Pour plus de détails, voir fluxus/docs/fluxus-documentation.txt
;;
;; Le scratchpad Fluxus possède deux modes d'opération, la console
;; (que vous voyez maintent) permet d'entrer des commandes et de voir
;; leurs résultat immédiatement. L'autre mode est l'éditeur qui s'apparente
;; plus à un éditeur de texte classique. Il y a 9 espaces de travail
;; qui permettent d'éditer plus d'un script à la fois. ctrl-1 jusqu'à ctrl-9
;; permet de basculer entre ces espaces, et ctrl-0 reviens sur la console.
;;
;; Pour copier/coller des exemples, enmener le curseur
;; jusque derrière l'invite de commande, naviguer jusqu'à l'exemple,
;; utiliser shift pour le selectionner, presser ctrl-c pour copier,
;; puis ctrl-0 et ctrl-v pour coller dans le buffer de texte.
;;
;; Autre sujets d'aide:
;; (help "keys") pour les commandes de control de fluxus au clavier
;; (help "console") pour plus d'aide sur la console
;; (help "editor") pour plus d'aide sur l'éditeur de livecoding
;; (help "camera") pour l'aide sur le control de la caméra
;; (help "language") pour plus d'infos sur les commandes Fluxus
;; (help "misc") pour diverses infos sur Fluxus
;; (help "toplap") pour le manifeste toplap
;; (help "authors") qui a fait cela?
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
;; ctrl-h : Hide/show the text.
;; ctrl-l : Load a new script (navigate with cursors and return).
;; ctrl-s : Save current script.
;; ctrl-d : Save as - current script (opens a filename dialog).
;; ctrl-p : Auto format the whitespace in your scheme script to be more pretty
;; ctrl-b : Show where cursor is.
;; ctrl-1 to 9 : Switch to selected workspace.
;; ctrl-0 : Switch to the REPL.
;; F3 : Resets the camera if you get lost.
;; F5 : (or ctrl-e) Execute the selected text, or all if none is selected.
;; F6 : Completely resets the interpreter, then executes the selected text,
;;      or all if none is selected.
;; F9 : Toggles scratchpad effects.
;; F10 : Decreases the text opacity
;; F11 : Increases the text opacity
;; F12 : Pauses keypress file playback.
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
;; ctrl-p : Auto format the whitespace in your scheme script to be more pretty
;; ctrl-b : Mostra onde o cursor está.
;; ctrl-1 to 9 : Vai para a área de trabalho selecionada.
;; ctrl-0 : Vai para o REPL.
;; F3 : Reinicia a câmera.
;; F6 : Reinicia completamente o interpretador, depois executa o texto selecionado,
;;      ou tudo se não tem nada selecionado.
;; F5 : Executa o texto selecionado, ou tudo se não tem nada selecionado.
;; F9 : Aciona os efeitos no scratchpad.
;; F10 : Diminui a opacidade do texto.
;; F11 : Aumenta a opacidade do texto.
;; F12 : Pausa as teclas durante playback.
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; keys
;; Retour:
;; Description:
;; Exemple:
;; Raccourcis Fluxus 
;; -----------
;;
;; ctrl-f : Mode plein-écran.
;; ctrl-w : Mode fenêtré.
;; ctrl-h : Cache/Montre le texte.
;; ctrl-l : Charge un nouveau script (navigation avec le curseur puis Entrée).
;; ctrl-s : Enregistrer le script en cours.
;; ctrl-d : Enregistrer sous - Ouvre une invite de nom de fichier.
;; ctrl-p : Auto-indentatino des espaces dans le script scheme pour plus de clarté
;; ctrl-b : Montre où se trouve le curseur.
;; ctrl-1 to 9 : Bascule entre les espace de travail.
;; ctrl-0 : Bascule vers le REPL.
;; F3 : Réinitialise la caméra en cas d'égarement.
;; F5 : (ou ctrl-e) Execute le texte sélectionné, ou tout si rien n'est sélectionné.
;; F6 : Réinitialise complètement l'interprèteur, puis exécute le texte sélectionné,
;;      ou tout si rien n'est sélectionnné.
;; F9 : Bascule les effets sur le scratchpad.
;; F10 : Diminue l'opacité du texte.
;; F11 : Augmente l'opacité du texte.
;; F12 : Met en pause la lecture d'un fichier d'enregistrement de touche (keypress).
;; EndFunctionDoc

;; StartFunctionDoc-en
;; console
;; Returns:
;; Description:
;; Example:
;; Fluxus console (or REPL)
;; ------------------------
;;
;; If you press ctrl and 0, instead of getting another script
;; workspace, you will be presented with a Read Evaluate Print
;; Loop interpreter, or repl for short. This is really just an
;; interactive interpreter similar to the commandline, where
;; you can enter scheme code for immediate evaluation. This code
;; is evaluated in the same interpreter as the other scripts, so
;; you can use the repl to debug or inspect global variables and
;; functions they define. This window is also where error
;; reporting is printed, along with the terminal window you
;; started fluxus from.
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

;; StartFunctionDoc-fr
;; console
;; Retour:
;; Description:
;; Exemple:
;; Fluxus console (ou REPL)
;; ------------------------
;;
;; Si vous pressez ctrl et 0, au lieu d'obtenir un nouvel espace
;; de script, vous serez en face du Read Evaluate Print
;; Loop interpreter, ou en abbrégé : REPL. Ceci s'apparente vraiment
;; à un interpréteur interactif, similaire à une ligne de commande,
;; où vous pouvez entrer du code Scheme pour une évaluation immédiate.
;; Ce code est évalué avec le même interpréteur
;; que celui des autres scripts. Vous pouvez donc utiliser le REPL
;; pour débugger ou inspecter les variables globales et les fonctions
;; que vous avez définies. Cette fenêtre est aussi l'endroit
;; où apparaitront les erreurs reportées, en même temps quand dans le terminal
;; duquel vous aurez lancé Fluxus.
;; EndFunctionDoc

;; StartFunctionDoc-en
;; editor
;; Returns:
;; Description:
;; Example:
;; Fluxus editor
;; -------------
;;
;; When using the fluxus scratchpad, the idea is that you only
;; need the one window to build scripts, or play live. f5 is the
;; key that runs the script when you are ready.  Selecting some
;; text (using shift) and pressing f5 will execute the selected
;; text only. This is handy for reevaluating functions without
;; running the whole script each time.
;;
;; Workspaces
;; ----------
;;
;; The script editor allows you to edit 9 scripts simultaneously
;; by using workspaces. To switch workspaces, use ctrl+number
;; key. Only one can be run at once though, hitting f5 will
;; execute the currently active workspace script.
;;
;; Auto focus
;; ----------
;;
;; The editor includes an auto scaling/centering feature which is
;; enabled by default. To disable it - add the line:
;; (set! fluxus-scratchpad-do-autofocus 0)
;; to your .fluxus.scm file - or create a new file called that in
;; your home directory, containing that line.
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; editor
;; Retorna:
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
;;
;; Auto foco
;; ---------
;;
;; O editor inclui uma propriedade de auto escalagem/centralização que
;; é ligada por padrão. Para desligar - adicione a linha:
;; (set! fluxus-scratchpad-do-autofocus 0)
;; para o seu arquivo .fluxus.scm - ou crie um novo arquivo chamado
;; assim no seu diretório home, contendo essa linha.
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; editor
;; Retour:
;; Description:
;; Exemple:
;; Fluxus editeur
;; -------------
;;
;; En utilisant le scratchpad Fluxus, l'idée est de n'utiliser
;; qu'une fenêtre pour construire, ou jouer en live. f5 est la
;; touche qui lance le script quand vous êtes prêt. En sélectionnant
;; du texte (en utilisant shift) et pressant f5, seulement
;; le texte sélectionné sera exécuté. Ceci est pratique pour
;; réévaluer des fonctions sans relancer tout le script à
;; chaque fois.
;;
;; Espaces de travail (Workspaces)
;; ----------
;;
;; L'éditeur de scripts permet jusqu'à 9 scripts simultanément
;; grâce aux espaces de travail. Pour basculer entre-eux,
;; utiliser ctrl+nombre. Un seul pour tourner en même temps,
;; en revanche, pressé f5 exécutera l'espace de travail actif.
;;
;; Auto focus
;; ----------
;;
;; L'éditeur inclus un centrage et mise à l'échelle automatique
;; acivé par defaut. Pour le désactiver - ajouter la line:
;; (set! fluxus-scratchpad-do-autofocus 0)
;; à votre fichier .fluxus.scm - ou créé en un nouveau dans
;; votre dossier utilisateur comprenant cette ligne.
;; EndFunctionDoc

;; StartFunctionDoc-en
;; camera
;; Returns:
;; Description:
;; Example:
;; Fluxus camera control
;; ---------------------
;;
;; The camera is controlled by moving the mouse and pressing
;; mouse buttons.
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

;; StartFunctionDoc-fr
;; camera
;; Retour:
;; Description:
;; Exemple:
;; Fluxus control caméra
;; ---------------------
;;
;; La caméra est controllée en bougeant et pressant les boutons
;; de la souris.
;;
;; Boutton Gauche souris: Rotation
;; Boutton Milieu souris: Translation
;; Boutton Droit souris: Zoom
;; EndFunctionDoc

;; StartFunctionDoc-en
;; misc
;; Returns:
;; Description:
;; Example:
;; Fluxus init script
;; ------------------
;; Fluxus looks for a script in your home directory called
;; .fluxus.scm which it will run if it is found. This is useful
;; for putting init commands (like connecting to jack or setting
;; the help text language etc)
;;
;; Frame rate throttling
;; ---------------------
;; By default fluxus throttles the framerate to around 40fps.
;; to disable this (and run at 100% cpu), use desiredfps with
;; some arbitrary large number:
;; (desiredfps 100000000)
;; To display the fps use (show-fps 1)
;;
;; Command line options
;; ---------------------
;; The easiest way to load a script into fluxus is to specify it on
;; the command line, eg:
;; $ fluxus myscript.scm
;; Will launch fluxus and load the script into the editor.
;; $ fluxus -x myscript.scm
;; Will launch fluxus, load, hide and execute the script.
;; Use -h to print all commandline options.
;;
;; Fluxus also contains a keypress and mouse event recorder for
;; recording livecoding sessions:
;; $ fluxus -r filename : record to keypresses file
;; $ fluxus -p filename : playback from file
;; $ fluxus -p filename -d time : seconds per frame time override for
;;                                playback (for use with frame-dump)
;;
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; misc
;; Retorna:
;; Descrição:
;; Exemplo:
;; Script init do fluxus
;; ---------------------
;; Fluxus procura por um script no seu diretório casa ($HOME) chamado
;; .fluxus.scm o qual vai ser executado se encontrado. Isto é útil
;; para colocar comandos de inicialização (como conectar ao jack ou
;; ajustar a linguagem para o texto de ajuda e etc)
;;
;; Aceleração da taxa de quadros
;; -----------------------------
;; Por padrão fluxus mantém a taxa de quadros em volta de 40 fps. para
;; desligar (e rodar em 100% de cpu), use desiredfps com algum número
;; arbitrariamente grande:
;; (desiredfps 1000000000)
;; Para mostrar a fps use (show-fps 1)
;;
;; Opções de linha de comando
;; --------------------------
;; O jeito mais fácil de carregar um script no fluxus é específicar o
;; mesmo na linha de comando, eg:
;; $ fluxus -x myscript.scm
;; Vai lançar o fluxus, carregar, esconder e executar o script.
;;
;; Fluxus também possui um gravador de teclas apertadas e cliques do
;; mouse para gravar sessões de códificação ao vivo (livecoding):
;; $ fluxus -r nome-do-arquivo : gravar em arquivo as teclas apertadas
;; $ fluxus -p nome-do-arquivo : tocar a partir do arquivo
;; $ fluxus -p nome-do-arquivo -d tempo : segundos por quadro
;; substituidos para tocar (pra usar com frame-dump)
;;
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; misc
;; Retour:
;; Description:
;; Exemple:
;; Fluxus script d'initialisation
;; ------------------
;; Fluxus recherche un script dans votre dossier utilisateur
;; appelé .fluxus.scm qui sera lancé s'il est trouvé. Il est
;; util pour y placer les commandes d'initialisation comme
;; la connection audio vers Jack ou la langue d'aide, etc.
;;
;; Etranglement du taux d'images
;; ---------------------
;; Par défaut Fluxus maintient le taux d'image aux alentours
;; de 40 fps. Pour le désactiver (et utiliser le cpu à 100%),
;; saisir la commande desiredfps avec un nombre large arbitraire:
;; (desiredfps 100000000)
;; Pour afficher les fps en cours (show-fps 1)
;;
;; Options de la ligne  de commande
;; ---------------------
;; La manière la plus simple de charge un script dans Fluxus est
;; de le spécifier avec la ligne de commande, par exemple:
;; $ fluxus monscript.scm
;; Fluxus démarrera et charge le script dans l'éditeur.
;; $ fluxus -x myscript.scm
;; Fluxus démarrera, charge, cache et exécuter le script.
;; Utiliser -h pour afficher toutes les options de la ligne de commande.
;;
;; FLuxus possède également un enregistreur des évenements de touches
;; du clavier et de la souris pour la captation des sessions de livecoding:
;; $ fluxus -r nom-de-fichier : enregistre vers le fihier de touches
;; $ fluxus -p nom-de-fichier : rejoue à partir du fichier
;; $ fluxus -p nom-de-fichier -d temps : secondes par temps d'images
                            pour la relecture avec (frame-dump)
;; EndFunctionDoc

;; StartFunctionDoc-en
;; authors
;; Returns:
;; Description:
;; Example:
;;
;; Authors
;; -------
;; Glauber Alex Dias Prado
;; Artem Baguinski
;; Dan Bethell
;; Nik Gaffney
;; Dave Griffiths
;; Claude Heiland-Allen
;; Alex Norman
;; Gabor Papp
;; Fabien Pelisson
;; Jeff Rose
;; James Tittle
;; Scott Watson
;; Evan Raskob
;;
;; "Computers are useless. They can only give you answers".
;;     Pablo Picasso (1881 - 1973).
;;
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; authors
;; Retorna:
;; Descrição:
;; Exemplo:
;;
;; Autores
;; -------
;; Glauber Alex Dias Prado
;; Artem Baguinski
;; Dan Bethell
;; Nik Gaffney
;; Dave Griffiths
;; Claude Heiland-Allen
;; Alex Norman
;; Gabor Papp
;; Fabien Pelisson
;; Jeff Rose
;; James Tittle
;; Scott Watson
;; Evan Raskob
;;
;; "Computadores são inúteis. Eles só podem te dar respostas".
;;     Pablo Picasso (1881 - 1973).
;;
;; EndFunctionDoc

;; StartFunctionDoc-en
;; authors
;; Returns:
;; Description:
;; Example:
;;
;; Authors
;; -------
;; Glauber Alex Dias Prado
;; Artem Baguinski
;; Dan Bethell
;; Nik Gaffney
;; Dave Griffiths
;; Claude Heiland-Allen
;; Alex Norman
;; Gabor Papp
;; Fabien Pelisson
;; Jeff Rose
;; James Tittle
;; Scott Watson
;; Evan Raskob
;;
;; "Les ordinateurs sont inutiles. Ils ne font que donner des réponses."
;;     Pablo Picasso (1881 - 1973).
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
;;
;; Scheme itself is out of the scope of this documentation,
;; but fluxus is a good way of learning it. I reccommend
;; "The Little Schemer" by Daniel P. Friedman and Matthias
;; Felleisen.
;;
;; The functions are grouped into sections to make things
;; a bit easier to find.
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

;; StartFunctionDoc-pt
;; linguagem
;; Retorna:
;; Descrição:
;; Exemplo:
;; Docs da Linguagem
;; -----------------
;;
;; Fluxus é composto de uma série de funções que extendem Scheme pra
;; uso em computação gráfica em tempo real.
;;
;; Scheme em si está fora do escopo desta documentação, mas fluxus é
;; uma boa forma de aprender ele. Eu recomendo "The Little Schemer"
;; por Daniel P. Friedman e Matthias Felleisen.
;;
;; As funções estão agrupadas em seções para fazer coisas um pouco
;; mais fáceis de encontrar.
;;
;; (help "sections")
;;    ... lista de seções ...
;; (help "maths")
;;    ... descrição e lista das funções matemáticas ...
;; (help "vmul")
;;    ... detalhes sobre a função com exemplos ...
;;
;;
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; language
;; Retour:
;; Description:
;; Exemple:
;; Langage Docs
;; -------------
;;
;; Fluxus est composé d'un ensemble de fonctions
;; qui étendent le Scheme pour créer des infographies
;; temps réel.
;;
;; Le Scheme en lui-même est hors-portée de cette documentation
;; mais Fluxus est une bonne manière de l'apprendre.
;; La lecture de "The Little Schemer" de Daniel P. Friedman et Matthias
;; Felleisen est recommandée.
;;
;; Ces fonctions sont groupées en sections pour les rendre un peu plus
;; faciles  à retrouver.
;;
;; (help "sections") pour la liste de toutes les sections.
;; (help "nom-de-la-section") pour en savoir plus sur la section.
;; (help "nom-de-la-fonction") pour en savoir plus sur une fonction.
;;
;; L'idée est que vous puissiez trouvez les fonctions intéressante
;; en effectuant quelque chose comme:
;;
;; (help "sections")
;;    ... liste des sections ...
;; (help "maths")
;;    ... description et liste des fonctions mathématiques ...
;; (help "vmul")
;;    ... details sur la fonction avec son exemple ...
;;
;;
;; EndFunctionDoc

;; StartFunctionDoc-en
;; toplap
;; Returns:
;; Description:
;; Example:
;; TOPLAP MANEFESTO
;; We demand:
;;  * Give us access to the performer's mind, to the whole human
;;    instrument.
;;  * Obscurantism is dangerous. Show us your screens.
;;  * Programs are instruments that can change themselves
;;  * The program is to be transcended - Artificial language is the way.
;;  * Code should be seen as well as heard, underlying algorithms viewed
;;    as well as their visual outcome.
;;  * Live coding is not about tools. Algorithms are thoughts.
;;    Chainsaws are tools. That's why algorithms are sometimes
;;    harder to notice than chainsaws.
;;
;; We recognise continuums of interaction and profundity, but prefer:
;;  * Insight into algorithms
;;  * The skillful extemporisation of algorithm as an
;;    expressive/impressive display of mental dexterity
;;  * No backup (minidisc, DVD, safety net computer)
;;
;; We acknowledge that:
;;  * It is not necessary for a lay audience to understand the
;;    code to appreciate it, much as it is not necessary to know
;;    how to play guitar in order to appreciate watching a guitar
;;    performance.
;;  * Live coding may be accompanied by an impressive display of
;;    manual dexterity and the glorification of the typing interface.
;;  * Performance involves continuums of interaction, covering
;;    perhaps the scope of controls with respect to the parameter
;;    space of the artwork, or gestural content, particularly
;;    directness of expressive detail. Whilst the traditional
;;    haptic rate timing deviations of expressivity in
;;    instrumental music are not approximated in code, why repeat
;;    the past? No doubt the writing of code and expression of
;;    thought will develop its own nuances and customs.
;;
;; Performances and events closely meeting these manifesto
;; conditions may apply for TOPLAP approval and seal.
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; toplap
;; Retorna:
;; Descrição:
;; Exemplo:
;; MANEFESTO TOPLAP
;; Nós queremos:
;;  * Nos dê acesso a mente do performante, a todo o instrumento humano.
;;  * Obscurantismo é perigoso. Nos mostre suas telas.
;;  * O Programa é pra ser transcendentalizado - Linguagem Artificial é
;; o caminho.
;;  * Programas são instrumentos que se alteram eles mesmo.
;;  * Código deve ser visto tanto quanto ouvido, algoritmos e sua
;; estrutura mostrados como seus resultados visuais.
;;  * Codificação ao vivo(livecoding) não é sobre ferramentas.
;;  * Algoritmos são pensamentos. Serras-elétrica são ferramentas.
;;  * É por isso que algoritmos são mais difíceis de notar que
;;  moto-serras.
;;
;; Nós reconhecemos continuos de interação e profundidade, mas
;; preferimos:
;;  * Introspecção nos algoritmos.
;;  * A habilidosa extemporização de algoritmos como uma amostra
;;  expressiva/impressiva da destreza mental.
;;  * Sem estoque (minidisc, DVD, computador seguro na net)
;;
;; Nos sabemos que:
;;  * Não é necessário para a audiência entender código para
;;  apreciá-lo, da mesma forma em que não é necessário saber tocar uma
;;  guitarra para gostar de assistir a uma perfomance de guitarra.
;;  * Códificação ao vivo pode ser acompanhado de uma impressionante
;;  mostra de destreza manual e a glorificação da interface de
;;  datilografia.
;;  * Perfomances envolvem continuos de interação, cobrindo talvez o
;;  escopo do controle a respeito dos parametros espaciais do trabalho
;;  artistico, ou conteudo gestual, particularmente direção de detalhe
;;  expressivo. Enquanto a tradicional taxa de tempo táctil variante
;;  de expressividade na música tradicional não são aproximadas no
;;  código, porque repetir o passado? Sem dúvida a escrita de código e
;;  expressão do pensamento daí derivado vai desenvolver suas próprias
;;  nuances e particularidades.
;;
;; Perfomances e eventos próximamente alcançando os critérios desse
;; manifesto podem aplicar pela aprovação e selo TOPLAP.
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; toplap
;; Retour:
;; Description:
;; Exemple:
;; MANIFESTE TOPLAP
;; Nous exigeons:
;;  * Donnez nous accès à l'esprit du créateur, l'instrument humain tout entier.
;;  * L'obscurentisme est dangereux. Montrez nous vos écrans.
;;  * Les programmes sont des instruments qui changent eux-mêmes/
;;  * Les programmes est à transcendenter - Le language artificiel est la voie.
;;  * Le code est à voir autant qu'à entendre, les algoritmes vus aussi bien
;;    que les visuels en aboutissants.
;;  * Le Codage Live n'est pas à propos d'outils. Les algorithmes sont pensées.
;;    Les tronçonneuses sont des outils. C'est pourquoi les algorithmes
;;    sont parfois plus difficiles à remarquer que les tronçonneuses.
;;
;; Nous reconnaissons les continuums d'interaction et de la profondeur, mais préfèront:
;;  * Un aperçu des algorithmes.
;;  * L'improvisation habile de l'algorithme comme un écran
;;    expressive / impressionnant de dextérité mentale.
;;  * Aucune sauvegarde 
;;  * No backup (minidisc, DVD, ordinateur)
;;
;; Nous reconnaissons que:
;;  * Il n'est pas nécessaire pour un public profane à comprendre le code
;;    pour l'apprécier, autant qu'il n'est pas nécessaire de savoir jouer
;;    de la guitare afin d'apprécier de regarder un spectacle guitare.
;;  * Le Codage Live peut être accompagné d'une impressionnante démonstration
;;    de dextérité manuelle et la glorification de l'interface de frappe.
;;  * La Performance implique des continuums d'interaction, couvrant peut-être
;;    la portée des contrôles à l'égard de l'espace des paramètres de l'oeuvre,
;;    ou le contenu gestuel, en particulier la franchise de détail expressif.
;;    Alors que les écarts de taux traditionnels haptique de l'expressivité
;;    dans la musique instrumentale ne sont pas approchés dans le code,
;;    pourquoi répéter le passé?
;;    Sans doute l'écriture de code et expression de la pensée dévellopera
;;    sa propres nuances et ses coutumes.
;;
;; Les performances et évenements réalisant dûement les closes de ce manifeste
;; pourront éventuellement recevoir l'approbation et le sceau de TOPLAP.
;; EndFunctionDoc



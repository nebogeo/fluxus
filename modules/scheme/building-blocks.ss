;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc

;; StartSectionDoc-pt
;; scheme-utils
;; Comandos do fluxus de alto nível escritos em fluxus
;; Exemplo:
;; EndSectionDoc

;; StartSectionDoc-fr
;; scheme-utils
;; Commandes haut-niveau de Fluxus écritent en Scheme.
;; Exemple:
;; EndSectionDoc

#lang racket/base
(require racket/list)
(require "fluxus-modules.ss")
(require "tasks.ss")
(provide
 with-state
 with-primitive
 with-pixels-renderer
 pdata-map!
 pdata-index-map!
 pdata-fold
 pdata-index-fold
 detach-parent
 vx vy vz vr vg vb va
 vx-set! vy-set! vz-set! vr-set! vg-set! vb-set! va-set!
 vadd vsub vmul vdiv mmul madd msub mdiv
 shader-set!
 )

;; StartFunctionDoc-en
;; detach-parent
;; Returns: void
;; Description:
;; Removes the parent for the current primitive, and fixes up the
;; transform so the primitive doesn't move. Use (parent 1) to avoid this fix up.
;; Example:
;; ; builds and animates a random heirarchical structure,
;; ; click on the objects to detach them from their parents
;; (define (build-heir depth)
;;     (with-state
;;         (let ((p (with-state
;;                         (translate (vector 2 0 0))
;;                         (scale 0.9)
;;                         (build-cube))))
;;             (when (> depth 0)
;;                 (parent p)
;;                 (for ((i (in-range 0 5)))
;;                     (when (zero? (random 3))
;;                         (rotate (vector 0 0 (* 45 (crndf))))
;;                         (build-heir (- depth 1))))))))
;;
;; (define (animate-heir children depth)
;;     (for-each
;;         (lambda (child)
;;             (with-primitive child
;;                 (rotate (vector 0 0 (sin (+ depth (time)))))
;;                 (animate-heir (get-children) (+ depth 1))))
;;         children))
;;
;; (define (animate)
;;     (animate-heir (get-children) 0)
;;     (when (mouse-button 1)
;;         (let ((s (select (mouse-x) (mouse-y) 2)))
;;             (when (not (zero? s))
;;                 (with-primitive s
;;                     (detach-parent))))))
;;
;; (clear)
;; (build-heir 5)
;; (every-frame (animate))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; detach-parent
;; Retorna: void
;; Descrição:
;; Remove o pai da primitiva atual, e ajusta as transformações para
;; que a primitiva não mova. Use (parent 1) para evitar este ajuste.
;; Exemplo:
;; ; builds and animates a random heirarchical structure,
;; ; click on the objects to detach them from their parents
;; (define (build-heir depth)
;;     (with-state
;;         (let ((p (with-state
;;                         (translate (vector 2 0 0))
;;                         (scale 0.9)
;;                         (build-cube))))
;;             (when (> depth 0)
;;                 (parent p)
;;                 (for ((i (in-range 0 5)))
;;                     (when (zero? (random 3))
;;                         (rotate (vector 0 0 (* 45 (crndf))))
;;                         (build-heir (- depth 1))))))))
;;
;; (define (animate-heir children depth)
;;     (for-each
;;         (lambda (child)
;;             (with-primitive child
;;                 (rotate (vector 0 0 (sin (+ depth (time)))))
;;                 (animate-heir (get-children) (+ depth 1))))
;;         children))
;;
;; (define (animate)
;;     (animate-heir (get-children) 0)
;;     (when (mouse-button 1)
;;         (let ((s (select (mouse-x) (mouse-y) 2)))
;;             (when (not (zero? s))
;;                 (with-primitive s
;;                     (detach-parent))))))
;;
;; (clear)
;; (build-heir 5)
;; (every-frame (animate))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; detach-parent
;; Retour: vide
;; Description:
;; Retire le parent de la primitive courante, et fixe sa transformation
;; afin qu'elle de bouge pas. Utiliser (parent 1) pour éviter la fixation.
;; Exemple:
;; ; Construis et anime une structure hiérarchique aléatoire,
;; ; cliquer sur les objets pour les détacher des parents
;; (define (build-heir depth)
;;     (with-state
;;         (let ((p (with-state
;;                         (translate (vector 2 0 0))
;;                         (scale 0.9)
;;                         (build-cube))))
;;             (when (> depth 0)
;;                 (parent p)
;;                 (for ((i (in-range 0 5)))
;;                     (when (zero? (random 3))
;;                         (rotate (vector 0 0 (* 45 (crndf))))
;;                         (build-heir (- depth 1))))))))
;;
;; (define (animate-heir children depth)
;;     (for-each
;;         (lambda (child)
;;             (with-primitive child
;;                 (rotate (vector 0 0 (sin (+ depth (time)))))
;;                 (animate-heir (get-children) (+ depth 1))))
;;         children))
;;
;; (define (animate)
;;     (animate-heir (get-children) 0)
;;     (when (mouse-button 1)
;;         (let ((s (select (mouse-x) (mouse-y) 2)))
;;             (when (not (zero? s))
;;                 (with-primitive s
;;                     (detach-parent))))))
;;
;; (clear)
;; (build-heir 5)
;; (every-frame (animate))
;; EndFunctionDoc

(define (detach-parent)
  (let ((m (get-global-transform)))
    (parent 1)
    (identity)
    (concat m)))

;; StartFunctionDoc-en
;; with-state expression ...
;; Returns: result of last expression
;; Description:
;; Encapsulates local state changes, and removes the need for push and pop.
;; Example:
;; ; state hierachy, by nesting with-state:
;; (with-state
;;    (hint-vertcols)
;;    (colour (vector 0 0 1))
;;    (with-state
;;        (translate (vector 1 0 0))
;;        (build-sphere 10 10))
;;     (build-torus 1 2 30 30))
;;
;; ; making primitives:
;; (define my-torus (with-state
;;    (hint-vertcols)
;;    (colour (vector 0 0 1))
;;    (build-torus 1 2 30 30)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; with-state expressão ...
;; Retorna: resultado da última expressão
;; Descrição:
;; Encapsula mudanças locais, e remove a necessidade de push e pop.
;; Exemplo:
;; ; state hierachy, by nesting with-state:
;; (with-state
;;    (hint-vertcols)
;;    (colour (vector 0 0 1))
;;    (with-state
;;        (translate (vector 1 0 0))
;;        (build-sphere 10 10))
;;     (build-torus 1 2 30 30))
;;
;; ; making primitives:
;; (define my-torus (with-state
;;    (hint-vertcols)
;;    (colour (vector 0 0 1))
;;    (build-torus 1 2 30 30)))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; with-state expression ...
;; Retour: Résultat de la dernière expresssion
;; Description:
;; Encapsule les changements d'états locaux, et évite ainsi l'utilisation de push et pop
;; Exemple:
;; ; hiérarchie d'états par imbrication de with-state
;; (with-state
;;    (hint-vertcols)
;;    (colour (vector 0 0 1))
;;    (with-state
;;        (translate (vector 1 0 0))
;;        (build-sphere 10 10))
;;     (build-torus 1 2 30 30))
;;
;; ; favrique des primitives:
;; (define my-torus (with-state
;;    (hint-vertcols)
;;    (colour (vector 0 0 1))
;;    (build-torus 1 2 30 30)))
;; EndFunctionDoc

(define-syntax with-state
  (syntax-rules ()
    ((_ a ...)
     (begin
       (push)
       (let ((r (begin a ...)))
         (pop)
         r)))))

;; StartFunctionDoc-en
;; with-primitive primitive expression ...
;; Returns: result of last expression
;; Description:
;; Encapsulates primitive state changes, and removes the need for grab and ungrab.
;; Example:
;; (define my-torus (with-state
;;    (colour (vector 0 0 1))
;;    (build-torus 1 2 30 30)))
;;
;; ; change the torus colour:
;; (with-primitive my-torus
;;    (colour (vector 0 1 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; with-primitive primitiva expressão ...
;; Retorna: resultado da última expressão
;; Descrição:
;; Encapsula mudanças de estado das primitivas, e remove a necessidade
;; de grab e ungrab.
;; Exemplo:
;; (define my-torus (with-state
;;    (colour (vector 0 0 1))
;;    (build-torus 1 2 30 30)))
;;
;; ; change the torus colour:
;; (with-primitive my-torus
;;    (colour (vector 0 1 0)))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; with-primitive primitive expression ...
;; Retour: Résultat de la dernière expression
;; Description:
;; Encapsule les changements d'états de primitives, et évite ainsi l'utilisation de grab et ungrab.
;; Exemple:
;; (define my-torus (with-state
;;    (colour (vector 0 0 1))
;;    (build-torus 1 2 30 30)))
;;
;; ; change la couleur de l'anneau:
;; (with-primitive my-torus
;;    (colour (vector 0 1 0)))
;; EndFunctionDoc

(define-syntax with-primitive
  (syntax-rules ()
    ((_ a b ...)
     (begin
       (grab a)
       (let ((r (begin b ...)))
         (ungrab)
         r)))))

;; StartFunctionDoc-en
;; with-pixels-renderer pixels-primitive expression ...
;; Returns: result of last expression
;; Description:
;; Allows you to render into a pixel primitive.
;; Example:
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; with-pixels-renderer primitiva-pixel expressão ...
;; Retorna: resultado da última expressão
;; Descrição:
;; Permite que você renderize em uma primitiva pixel.
;; Exemplo:
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; with-pixels-renderer pixels-primitive expression ...
;; Retour: Résultat de la dernière expression
;; Description:
;; Permet d'envoyer un rendu dans une primitive pixel
;; Exemple:
;; EndFunctionDoc

(define-syntax with-pixels-renderer
  (syntax-rules ()
    ((_ a b ...)
     (begin
       (renderer-grab a)
       (let/ec out
           ;; handle errors
           (let ([renderer-error
                   (lambda (e)
                     (printf "Error in with-pixels-renderer '~a - render target restored.~%" a)
           (print-error e)
                     (renderer-ungrab)
                     (out #t))])
                 (call-with-exception-handler renderer-error
                                              (lambda ()
                                                (let ([r (begin b ...)])
                                                  (renderer-ungrab)
                                                  r)))
           ))
     ))))

;; StartFunctionDoc-en
;; with-ffgl ffgl-pluginid expression ...
;; Returns: result of last expression
;; Description:
;; Allows you to work with the specified FFGL plugin.
;; Example:
;; (clear)
;; (define plugin (ffgl-load "FFGLTile.dylib" 256 256))
;;
;; (with-ffgl plugin
;;   (for ([i (ffgl-get-info)])
;;        (printf "~a~n" i)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; with-ffgl pluginid-ffgl expressão ...
;; Retorna: resultado da última expressão
;; Descrição:
;; Permite que você trabalhe no plugin FFGL específicado.
;; Exemplo:
;; (clear)
;; (define plugin (ffgl-load "FFGLTile.dylib" 256 256))
;;
;; (with-ffgl plugin
;;   (for ([i (ffgl-get-info)])
;;        (printf "~a~n" i)))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; with-ffgl ffgl-pluginid expression ...
;; Retour: Résultat de la dernière expression
;; Description:
;; Permet de travailler avec le plugin FFGL spécifié.
;; Exemple:
;; (clear)
;; (define plugin (ffgl-load "FFGLTile.dylib" 256 256))
;;
;; (with-ffgl plugin
;;   (for ([i (ffgl-get-info)])
;;        (printf "~a~n" i)))
;; EndFunctionDoc

(define-syntax with-ffgl
  (syntax-rules ()
    ((_ a b ...)
     (begin
       (ffgl-push a)
       (let ((r (begin b ...)))
         (ffgl-pop)
         r)))))

;; StartFunctionDoc-en
;; pdata-map! procedure read/write-pdata-name read-pdata-name ...
;; Returns: void
;; Description:
;; A high level control structure for simplifying passing over pdata arrays for
;; primitive deformation. Should be easier and less error prone than looping manually.
;; Writes to the first pdata array.
;; Example:
;; (clear)
;; (define my-torus (build-torus 1 2 30 30))
;;
;; (with-primitive my-torus
;;   (pdata-map!
;;      (lambda (position)
;;          (vadd position (vector (flxrnd) 0 0))) ; jitter the vertex in x
;;      "p")) ; read/write the position pdata array
;;
;; (with-primitive my-torus
;;   (pdata-map!
;;      (lambda (position normal)
;;          (vadd position normal)) ; add the normal to the position (expand the object)
;;      "p" "n")) ; read/write the position pdata array, read the normals array
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; pdata-map! procedimento nome-pdata-leitura/escrita nome-pdata-leitura ...
;; Retorna: void
;; Descrição:
;; Uma estrutura de controle de alto nível para simplificar a passagem sobre uma array de
;; pdata para deformação de primitivas. Deve ser mais fácil e menos sujeito a erros do que fazer
;; o loop manualmente. Escreve na primeira array pdata.
;; Exemplo:
;; (clear)
;; (define my-torus (build-torus 1 2 30 30))
;;
;; (with-primitive my-torus
;;   (pdata-map!
;;      (lambda (position)
;;          (vadd position (vector (flxrnd) 0 0))) ; jitter the vertex in x
;;      "p")) ; read/write the position pdata array
;;
;; (with-primitive my-torus
;;   (pdata-map!
;;      (lambda (position normal)
;;          (vadd position normal)) ; add the normal to the position (expand the object)
;;      "p" "n")) ; read/write the position pdata array, read the normals array
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; pdata-map! procedure lecture/ecriture-nom-pdata lecture-nom-pdata ...
;; Retour: vide
;; Description:
;; Structure de control haut-niveau pour simplifier le passage en revu des valeurs pdata
;; pour la déformation de primitives. Doit être plus facile et générer moins d'erreurs que la récursion manuelle.
;; Ecriture sur le premier tableau de pdata en argument
;; Exemple:
;; (clear)
;; (define my-torus (build-torus 1 2 30 30))
;;
;; (with-primitive my-torus
;;   (pdata-map!
;;      (lambda (position)
;;          (vadd position (vector (flxrnd) 0 0))) ; agite les vertex en x
;;      "p")) ; lecture/ecriture du tableau pdata de positions.
;;
;; (with-primitive my-torus
;;   (pdata-map!
;;      (lambda (position normal)
;;          (vadd position normal)) ; ajout des normales aux positions (accroit l'objet)
;;      "p" "n")) ; lecture/ecriture du tableau pdata de positions. lecture du tableau de normales.
;; EndFunctionDoc

(define-syntax pdata-map!
  (syntax-rules ()
    ((_ proc pdata-write-name pdata-read-name ...)
     (letrec
         ((loop (lambda (n total)
                  (cond ((not (> n total))
                         (pdata-set! pdata-write-name n
                                     (proc (pdata-ref pdata-write-name n)
                                           (pdata-ref pdata-read-name n) ...))
                         (loop (+ n 1) total))))))
       (loop 0 (- (pdata-size) 1))))))

;; StartFunctionDoc-en
;; pdata-index-map! procedure read/write-pdata-name read-pdata-name ...
;; Returns: void
;; Description:
;; A high level control structure for simplifying passing over pdata arrays for
;; primitive deformation. Same as pdata-map! except pdata-index-map! supplies
;; the index of the current pdata element as the first argument to 'procedure'.
;; Example:
;; (clear)
;; (define my-torus (build-torus 1 2 30 30))
;;
;; (with-primitive my-torus
;;   (pdata-index-map!
;;      (lambda (index position)
;;          (vadd position (vector (gh index) 0 0))) ; jitter the vertex in x
;;      "p")) ; read/write the position pdata array
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; pdata-index-map! procedimento nome-pdata-leitura/escrita nome-pdata-leitura ...
;; Retorna: void
;; Descrição:
;; Uma estrutura de controle de alto nível para simplificar a passagem sobre uma array de
;; pdata para deformação de primitivas. Mesmo que pdata-map! exceto que pdata-index-map! fornece
;; o índice do elemento pdata atual como o primeiro argumento ao "procedimento".
;; Exemplo:
;; (clear)
;; (define my-torus (build-torus 1 2 30 30))
;;
;; (with-primitive my-torus
;;   (pdata-index-map!
;;      (lambda (index position)
;;          (vadd position (vector (gh index) 0 0))) ; jitter the vertex in x
;;      "p")) ; read/write the position pdata array
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; pdata-index-map! procedure lecture/écriture-nom-pdata lecture-nom-pdata ...
;; Retour: vide
;; Description:
;; Structure de control haut-niveau pour simplifier le passage en revu des valeurs pdata
;; pour la déformation de primitives. Identique à pdata-map! à l'exception que pdata-index-map!
;; fournit l'index de l'élément en cours comme premier argument de la 'procédure'.
;; Exemple:
;; (clear)
;; (define my-torus (build-torus 1 2 30 30))
;;
;; (with-primitive my-torus
;;   (pdata-index-map!
;;      (lambda (index position)
;;          (vadd position (vector (gh index) 0 0))) ; agite les vertex en x
;;      "p")) ; lecture/ecriture du tableau pdata de positions.
;; EndFunctionDoc

(define-syntax pdata-index-map!
  (syntax-rules ()
    ((_ proc pdata-write-name pdata-read-name ...)
     (letrec
         ((loop (lambda (n total)
                  (cond ((not (> n total))
                         (pdata-set! pdata-write-name n
                                     (proc n (pdata-ref pdata-write-name n)
                                           (pdata-ref pdata-read-name n) ...))
                         (loop (+ n 1) total))))))
       (loop 0 (- (pdata-size) 1))))))

;; StartFunctionDoc-en
;; pdata-fold procedure start-value read-pdata-name ...
;; Returns: result of folding procedure over pdata array
;; Description:
;; A high level control structure for doing calculations on pdata arrays.
;; Runs the procedure over each pdata element accumulating the result.
;; Should be easier and less error prone than looping manually.
;; Example:
;; (define my-torus (build-torus 1 2 30 30))
;;
;; ; find the centre of the primitive by averaging
;; ; the points position's together
;; (let ((centre
;;        (with-primitive my-torus
;;                        (vdiv (pdata-fold
;;                               vadd
;;                               (vector 0 0 0)
;;                               "p") (pdata-size)))))
;;
;;   (display centre)(newline))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; pdata-fold procedimento valor-inicial nome-pdata-leitura ...
;; Retorna: resultado do procedimento de dobragem sobre a array pdata
;; Descrição:
;; Uma estrutura de controle de alto nível para fazer cálculos em arrays pdata.
;; Roda o procedimento sobre cada elemento pdata acumulando o resultado.
;; Deve ser mais fácil e menos sujeito a erros que percorrer manualmente.
;; Exemplo:
;; (define my-torus (build-torus 1 2 30 30))
;;
;; ; find the centre of the primitive by averaging
;; ; the points position's together
;; (let ((centre
;;        (with-primitive my-torus
;;                        (vdiv (pdata-fold
;;                               vadd
;;                               (vector 0 0 0)
;;                               "p") (pdata-size)))))
;;
;;   (display centre)(newline))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; pdata-fold procedure valeur-départ lecture-nom-pdata ...
;; Retour: Résultat de la procédure à cumul sur le tableau pdata
;; Description:
;; Structure de control de haut-niveau pour effectuer des calculs sur tableau pdata.
;; Effectue la procédure sur chaque élément pdata en accumulant le résultat.
;; Doit être plus facile et généré moins d'erreurs que la récursion manuelle.
;; Exemple:
;; (define my-torus (build-torus 1 2 30 30))
;;
;; ; recherche du centre de la primitive en moyennant
;; ; ensemble la position des points
;; (let ((centre
;;        (with-primitive my-torus
;;                        (vdiv (pdata-fold
;;                               vadd
;;                               (vector 0 0 0)
;;                               "p") (pdata-size)))))
;;
;;   (display centre)(newline))
;; EndFunctionDoc

(define-syntax pdata-fold
  (syntax-rules ()
    ((_ proc start pdata-read-name ...)
     (letrec
         ((loop (lambda (n total current)
                  (cond ((> n total) current)
                        (else
                         (proc (pdata-ref pdata-read-name n) ...
                               (loop (+ n 1) total current)))))))
       (loop 0 (- (pdata-size) 1) start)))))

;; StartFunctionDoc-en
;; pdata-index-fold procedure start-value read-pdata-name ...
;; Returns: result of folding procedure over pdata array
;; Description:
;; Same as pdata-fold except it passes the index of the current pdata
;; element as the first parameter of 'procedure'.
;; Example:
;; (define my-torus (build-torus 1 2 30 30))
;;
;; ; can't think of a good example for this yet...
;; (let ((something
;;        (with-primitive my-torus
;;                        (vdiv (pdata-index-fold
;;                               (lambda (index position ret)
;;                                   (vadd ret (vmul position index)))
;;                               (vector 0 0 0)
;;                               "p") (pdata-size)))))
;;
;;   (display something)(newline))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; pdata-index-fold procedimento valor-inicial nome-pdata-leitura ...
;; Retorna: resultado do procedimento de dobragem sobre a array pdata
;; Descrição:
;; Igual a pdata-fold exceto que passa o índice do elemento pdata atual
;; como o primeiro parâmetro de "procedimento".
;; Exemplo:
;; (define my-torus (build-torus 1 2 30 30))
;;
;; ; can't think of a good example for this yet...
;; (let ((something
;;        (with-primitive my-torus
;;                        (vdiv (pdata-index-fold
;;                               (lambda (index position ret)
;;                                   (vadd ret (vmul position index)))
;;                               (vector 0 0 0)
;;                               "p") (pdata-size)))))
;;
;;   (display something)(newline))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; pdata-index-fold procedure valeur-départ lecture-nom-pdata ...
;; Retour: Résultat de la procédure à cumul sur le tableau pdata
;; Description:
;; Identique à pdata-fold, à l'exception que l'index de l'élement pdata en cours
;; est passé en premier argument de la 'procedure'.
;; Exemple:
;; (define my-torus (build-torus 1 2 30 30))
;;
;; ; Pas encore trouvé de meilleur exemple pour ceci...
;; (let ((something
;;        (with-primitive my-torus
;;                        (vdiv (pdata-index-fold
;;                               (lambda (index position ret)
;;                                   (vadd ret (vmul position index)))
;;                               (vector 0 0 0)
;;                               "p") (pdata-size)))))
;;
;;   (display something)(newline))
;; EndFunctionDoc

(define-syntax pdata-index-fold
  (syntax-rules ()
    ((_ proc start pdata-read-name ...)
     (letrec
         ((loop (lambda (n total current)
                  (cond ((> n total) current)
                        (else
                         (proc n (pdata-ref pdata-read-name n) ...
                               (loop (+ n 1) total current)))))))
       (loop 0 (- (pdata-size) 1) start)))))

;; shorthand helpers
(define (vx v) (vector-ref v 0))
(define (vy v) (vector-ref v 1))
(define (vz v) (vector-ref v 2))
(define (vw v) (vector-ref v 3))

(define (vr v) (vector-ref v 0))
(define (vg v) (vector-ref v 1))
(define (vb v) (vector-ref v 2))
(define (va v) (vector-ref v 3))

(define (vx-set! v s) (vector-set! v 0 s))
(define (vy-set! v s) (vector-set! v 1 s))
(define (vz-set! v s) (vector-set! v 2 s))
(define (vw-set! v s) (vector-set! v 3 s))

(define (vr-set! v s) (vector-set! v 0 s))
(define (vg-set! v s) (vector-set! v 1 s))
(define (vb-set! v s) (vector-set! v 2 s))
(define (va-set! v s) (vector-set! v 3 s))

;; StartFunctionDoc-en
;; vadd vector vector ...
;; Returns: result-vector
;; Description:
;; Adds vectors together
;; Example:
;; (vadd (vector 1 2 3) (vector 5 2 7))
;; (vadd (vector 1 2 3 4) (vector 7 1 1 4))
;; (vadd (vector 1 2) (vector 3 3) (vector 5 5))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; vadd vecteur vecteur ...
;; Retour: vecteur-résultant
;; Description:
;; Additionne ensemble des vecteurs
;; Exemple:
;; (vadd (vector 1 2 3) (vector 5 2 7))
;; (vadd (vector 1 2 3 4) (vector 7 1 1 4))
;; (vadd (vector 1 2) (vector 3 3) (vector 5 5))
;; EndFunctionDoc

(define (vadd2 a b)
  (build-vector (vector-length a)
                (lambda (i)
                  (+ (vector-ref a i)
                     (vector-ref b i)))))

(define-syntax vadd
  (syntax-rules ()
    [(vadd a b) (vadd2 a b)]
    [(vadd a b c ...) (vadd2 a (vadd b c ...))]))

;; StartFunctionDoc-en
;; vsub vector vector ...
;; Returns: result-vector
;; Description:
;; Subtracts a vector or multiple vectors from each other
;; Example:
;; (vsub (vector 1 2 3) (vector 5 2 7))
;; (vsub (vector 1 2 3 4) (vector 7 1 1 4))
;; (vsub (vector 1 2) (vector 3 3) (vector 5 5))
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; vsub vecteur vecteur ...
;; Retour: vecteur-résultant
;; Description:
;; Soustrait un ou plusieurs vecteurs entre eux.
;; Exemple:
;; (vsub (vector 1 2 3) (vector 5 2 7))
;; (vsub (vector 1 2 3 4) (vector 7 1 1 4))
;; (vsub (vector 1 2) (vector 3 3) (vector 5 5))
;; EndFunctionDoc

(define (vsub2 a b)
  (build-vector (vector-length a)
                (lambda (i)
                  (- (vector-ref a i)
                     (vector-ref b i)))))

(define-syntax vsub
  (syntax-rules ()
    [(vsub a b) (vsub2 a b)]
    [(vsub a b c ...) (vsub2 a (vadd b c ...))]))

;; StartFunctionDoc-en
;; vmul vector number
;; Returns: result-vector
;; Description:
;; Multiplies a vector by a number
;; Example:
;; (vmul (vector 1 2 3) 2)
;; (vmul (vector 1 2 3 4 5) 3)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; vmul vetor número
;; Retorna: vetor resultante
;; Descrição:
;; Multiplica um vetor por um número.
;; Exemplo:
;; (vmul (vector 1 2 3) 2)
;; (vmul (vector 1 2 3 4 5) 3)
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; vmul vecteur nombre
;; Retour: vecteur-résultant
;; Description:
;; Multiplie un vecteur par un nombre
;; Exemple:
;; (vmul (vector 1 2 3) 2)
;; (vmul (vector 1 2 3 4 5) 3)
;; EndFunctionDoc

(define (vmul a m)
  (build-vector (vector-length a)
                (lambda (i)
                  (* (vector-ref a i) m))))

;; StartFunctionDoc-en
;; vdiv vector number
;; Returns: result-vector
;; Description:
;; Divides a vector by a number
;; Example:
;; (vdiv (vector 1 2 3) 2)
;; (vdiv (vector 1 2 3 4 5) 3)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; vdiv vetor número
;; Retorna: vetor resultante
;; Descrição:
;; Divide um vetor por um número
;; Exemplo:
;; (vdiv (vector 1 2 3) 2)
;; (vdiv (vector 1 2 3 4 5) 3)
;; EndFunctionDoc

;; StartFunctionDoc-fr
;; vdiv vecteur nombre
;; Retour: vecteur-résultant
;; Description:
;; Divise un vecteur par un nombre
;; Exemple:
;; (vdiv (vector 1 2 3) 2)
;; (vdiv (vector 1 2 3 4 5) 3)
;; EndFunctionDoc

(define (vdiv a m)
  (build-vector (vector-length a)
                (lambda (i)
                  (/ (vector-ref a i) m))))


; macros for mmul, madd, mdiv, msub allowing
; them to take arbitrary counts of arguments

(define-syntax mmul
  (syntax-rules ()
    ((_ a ...)
      (mmul-list (list a ...)))))

(define (mmul-list l)
  (cond
    ((eq? (length l) 1) (car l))
    ((eq? (length l) 2) (mmul2 (car l) (cadr l)))
    (else (mmul2 (car l) (mmul-list (cdr l))))))

(define-syntax madd
  (syntax-rules ()
    ((_ a ...)
      (madd-list (list a ...)))))

(define (madd-list l)
  (cond
    ((eq? (length l) 1) (car l))
    ((eq? (length l) 2) (madd2 (car l) (cadr l)))
    (else (madd2 (car l) (madd-list (cdr l))))))

(define-syntax msub
  (syntax-rules ()
    ((_ a ...)
      (msub-list (list a ...)))))

(define (msub-list l)
  (cond
    ((eq? (length l) 1) (car l))
    ((eq? (length l) 2) (msub2 (car l) (cadr l)))
    (else (msub2 (car l) (msub-list (cdr l))))))

(define-syntax mdiv
  (syntax-rules ()
    ((_ a ...)
      (mdiv-list (list a ...)))))

(define (mdiv-list l)
  (cond
    ((eq? (length l) 1) (car l))
    ((eq? (length l) 2) (mdiv2 (car l) (cadr l)))
    (else (mdiv2 (car l) (mdiv-list (cdr l))))))

;; shader-set! with keyword arguments
(define shader-set!
  (make-keyword-procedure
    (lambda (kws kw-args . rest)
        (shader-list-set!
            (flatten
				(append
					rest
					(for/list ([kw kws]
							   [arg kw-args])
						(list (keyword->string kw) arg))))))))


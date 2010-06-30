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

#lang racket/base
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
 vadd vsub mmul madd msub mdiv
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

; macros for vadd, vsub, mmul, madd, mdiv, msub allowing
; them to take arbitrary counts of arguments

(define-syntax vadd
  (syntax-rules ()
    ((_ a ...)
      (vadd-list (list a ...)))))

(define (vadd-list l)
  (cond
    ((eq? (length l) 1) (car l))
    ((eq? (length l) 2) (vadd2 (car l) (cadr l)))
    (else (vadd2 (car l) (vadd-list (cdr l))))))

(define-syntax vsub
  (syntax-rules ()
    ((_ a ...)
      (vsub-list (list a ...)))))

(define (vsub-list l)
  (cond
    ((eq? (length l) 1) (car l))
    ((eq? (length l) 2) (vsub2 (car l) (cadr l)))
    (else (vsub2 (car l) (vadd-list (cdr l))))))

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



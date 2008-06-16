;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; fluxus-building-blocks
;; A new and fairly untested set of higher level control structures for manipulating
;; objects and state in fluxus in a cleaner and safer manner.
;; Example:
;; EndSectionDoc 

;; StartSectionDoc-pt
;; blocos-de-construcao-fluxus
;; Um novo e pouco testado ajuste de estruturas de controle de ordem maior para 
;; manipular objetos e estados e estado no fluxus de uma maneira mais limpa e segura.
;; Exemplo:
;; EndSectionDoc 

#lang scheme/base
(require "fluxus-engine.ss")
(provide 
 with-state
 with-primitive
 pdata-map!
 pdata-index-map!
 pdata-fold
 pdata-index-fold
 )

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
;;        (transform (vector 1 0 0))
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
;; Encapsula mudanças locais, e remove a necessidade por push e pop.
;; Exemplo:
;; ; state hierachy, by nesting with-state:
;; (with-state
;;    (hint-vertcols)
;;    (colour (vector 0 0 1))
;;    (with-state
;;        (transform (vector 1 0 0))
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
;; Encapsula mudanças de estado das primitivas
;; Exemplo:
;; 
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
;;          (vadd position (vector (flxrnd) 0 0))) ;; jitter the vertex in x
;;      "p")) ;; read/write the position pdata array
;; 
;; (with-primitive my-torus
;;   (pdata-map!
;;      (lambda (position normal)
;;          (vadd position normal)) ;; add the normal to the position (expand the object)
;;      "p" "n")) ;; read/write the position pdata array, read the normals array
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
;;          (vadd position (vector (flxrnd) 0 0))) ;; jitter the vertex in x
;;      "p")) ;; read/write the position pdata array
;; 
;; (with-primitive my-torus
;;   (pdata-map!
;;      (lambda (position normal)
;;          (vadd position normal)) ;; add the normal to the position (expand the object)
;;      "p" "n")) ;; read/write the position pdata array, read the normals array
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
;;   (pdata-map!
;;      (lambda (index position)
;;          (vadd position (vector (gh index) 0 0))) ;; jitter the vertex in x
;;      "p")) ;; read/write the position pdata array
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
;;   (pdata-map!
;;      (lambda (index position)
;;          (vadd position (vector (gh index) 0 0))) ;; jitter the vertex in x
;;      "p")) ;; read/write the position pdata array
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
;; ;; find the centre of the primitive by averaging 
;; ;; the points position's together
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
;; ;; find the centre of the primitive by averaging 
;; ;; the points position's together
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
;; ;; can't think of a good example for this yet...
;; (let ((something 
;;        (with-primitive my-torus
;;                        (vdiv (pdata-fold
;;                               (lambda (index position)
;;                                   (vmul position index))
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
;; ;; can't think of a good example for this yet...
;; (let ((something 
;;        (with-primitive my-torus
;;                        (vdiv (pdata-fold
;;                               (lambda (index position)
;;                                   (vmul position index))
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



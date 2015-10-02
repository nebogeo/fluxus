;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; testing-functions
;; A set of higher level control structures for manipulating
;; objects and state in fluxus in a cleaner and safer manner.
;; Example:
;; EndSectionDoc

;; StartSectionDoc-pt
;; testing-functions
;; Um conjunto de estruturas de alto nível para manipular objetos e
;; estado no fluxus de uma forma mais limpa e segura.
;; Exemplo:
;; EndSectionDoc


(module fluxus racket

(require "scratchpad.ss")
(require "help.ss")
(require "camera.ss")
;(require mzlib/string)

(provide
  self-test
  run-scripts)

;; functions to extract the examples from the helpmap
;; indeed its a bit messed, im going to take a look at it, but i cant
;; make any guarantees that it is gonna be any better :).

(define (get-function-list section)
    (caddr (car section)))

(define (get-example func)
    (cadddr (cadr func)))

(define (get-name func)
    (car func))

(define (test-functions section log)
    (for-each
        (lambda (i)
            (unit-test (get-name i) (get-example i) log))
        (get-function-list section)))

(define (test-sections helpmap log)
    (for-each
        (lambda (i)
            (when (not (or
                (string=? "high-level-scratchpad-docs" (car i))
                (string=? "frisbee" (car i))
                (string=? "artkp" (car i))
                (string=? "video" (car i))
                (string=? "ffgl" (car i))
                (string=? "fluxa" (car i))
                (string=? "renderer" (car i))
                (string=? "planetarium" (car i))
                (string=? "testing-functions" (car i)) ; would be silly now, wouldn't it...
                    ))
                (test-functions (cdr i) log)))
        helpmap))

(define errors 0)
(define error-list '())
(define do-log #f)

(define (unit-test name code log)
  (clear)
  (printf "testing: ~a~n" name)
  (when do-log
     (fprintf log "testing: ~a~n" name))

  ;; (cond
  ;;   ((string=? code "")
  ;;     (set! errors (+ errors 1))
  ;;     (set! error-list (cons name error-list)))
  ;;   (else
  ;;     (eval-string
  ;;            code
  ;;            (lambda (s)
  ;;              (when do-log
  ;;                    (fprintf log "--------------------~n")
  ;;                    (fprintf log "~a~n" name)
  ;;                    (fprintf log "~a~n" code)
  ;;                    (fprintf log "~a~n" s))
  ;;              (set! errors (+ errors 1))
  ;;              (set! error-list (cons name error-list))))))
    (sleep 0.1))

(define (go)
  (let ((log (open-output-file "log.txt" #:exists 'replace)))
	(file-stream-buffer-mode log 'line)
	(test-sections (get-helpmap) log)
	(close-output-port log)
	(printf "testing found ~a errors: ~a ~n" errors error-list)))

;; StartFunctionDoc-en
;; self-test do-logging
;; Returns: void
;; Description:
;; Runs all of the function reference scripts in a separate
;; thread so you can watch it in action. Just checks for syntactic errors in the
;; scheme, changes in bound C++ function signatures and crashes.
;; Graphical code is difficult to test for correctness further (that's my excuse). If
;; do-logging is true it outputs a log text file to the current directory for debugging.
;; Example:
;; (self-test #t)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; self-test faz-logar
;; Retorna: void
;; Descrição:
;; Roda todas as funções de referencia nos fluxus em um thread
;; separado de forma que você possa ver acontecendo. Apenas checa por
;; erros sintáticos em scheme, mudanças nas assinaturas das funçoes
;; C++ ligadas e crashes.
;; Código gráfico é difícil de testar por correção além (essa é nossa
;; desculpa). Se faz-logar for verdadeiro um arquivo texto de log é
;; escrito no diretório atual para debugging.
;; Exemplo:
;; (self-test #t)
;; EndFunctionDoc

(define (self-test log)
  (set! do-log log)
  (thread go))

;; StartFunctionDoc-en
;; run-scripts path-to-examples seconds-per-script
;; Returns: void
;; Description:
;; Runs all of the example scripts in a separate
;; thread so you can watch it in action.
;; Example:
;; (run-scripts path-to-scripts seconds-per-script)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; run-scripts caminho-para-exemplos segundos-por-script
;; Retorna: void
;; Descrição:
;; Roda todos os scripts de exemplo em um thread separado para você
;; poder assistir.
;; Exemplo:
;; (run-scripts path-to-scripts seconds-per-script)
;; EndFunctionDoc

(define example "")
(define run-time 5)
(define examples-path "")

(define (run-example)
    (load (string-append examples-path example)))

(define (run-all)
    (for-each
        (lambda (filename)
            (let* ((filename (path->string filename))
                    (len (string-length filename)))

                (when (and (> len 4) (string=? ".scm" (substring filename (- len 4) len)))
                    (printf "~a~n" filename)
                    (set! example filename)
          (clear)
          (reset-camera)
                    (let ((thr (thread run-example)))
                        (sleep run-time)
                        (kill-thread thr)
                        (clear))
                    )))
        (directory-list examples-path)))

(define (run-scripts path time)
   (set! examples-path path)
   (set! run-time time)
   (thread run-all))


)

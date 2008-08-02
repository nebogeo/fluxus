;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; Stuff for poly primitives
;; Example:
;; EndSectionDoc 

#lang scheme/base
(require "fluxus-engine.ss")
(require "building-blocks.ss")
(provide 
	poly-for-each-face)

;; StartFunctionDoc-en
;; pdata-for-each-face proc pdatanames
;; Returns: list of pdata values
;; Description:
;; Calls proc with the pdata for each face in a list - assumes indexed triangle list atm...
;; Example:  
;; EndFunctionDoc 

(define (poly-for-each-face proc pdatanames)
    (let ((face '()))
        (for-each
            (lambda (i)   
                (let ((vert-data (pdata-slice i pdatanames)))
                    (cond 
                         ((< (length face) 2)
                             (set! face (cons vert-data face)))
                         (else
                             (proc (cons vert-data face))
                             (set! face '())))))
                (poly-indices))))
	

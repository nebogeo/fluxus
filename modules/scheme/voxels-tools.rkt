;; [ Copyright (C) 2010 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; voxels-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc

;; StartSeciontDoc-pt
;; voxels-utils
;; Comandos fluxus de alto nível escritos em scheme.
;; Exemplo:
;; EndFunctionDoc


(module fluxus racket

(require "fluxus-modules.ss")
(require "building-blocks.ss")
(require "maths.ss")
(provide
	voxels-index
    voxels-pos)

;; StartFunctionDoc-en
;; voxel-index position-vector
;; Returns: index-number
;; Description:
;; Returns the pdata index for the voxel position
;; Example:
;; (with-primitive (build-voxels 10 10 10)
;;     (display (voxels-index (vector 5 5 5)))(newline))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; voxel-index vetor-posição
;; Retorna: número-index
;; Descrição:
;; Retorna o índice pdata para a posição voxel.
;; Exemplo:
;; (with-primitive (build-voxels 10 10 10)
;;     (display (voxels-index (vector 5 5 5)))(newline))
;; EndFunctionDoc

(define (voxels-index v)
    (+ (vx v) (* (vy v) (voxels-width)) (* (vz v) (voxels-width) (voxels-height))))

;; StartFunctionDoc-en
;; voxels-pos index
;; Returns: position-vector
;; Description:
;; Returns the voxel position for the given pdata index
;; Example:
;; (with-primitive (build-voxels 10 10 10)
;;     (display (voxels-pos 200))(newline))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; voxels-pos indice
;; Retorna: vetor-posição
;; Descrição:
;; Retorna a posição voxel para o índice pdata dado.
;; Exemplo:
;; (with-primitive (build-voxels 10 10 10)
;;     (display (voxels-pos 200))(newline))
;; EndFunctionDoc

(define (voxels-pos i)
    (vector (modulo i (voxels-width))
            (modulo (quotient i (voxels-width)) (voxels-height))
            (quotient i (* (voxels-width) (voxels-height)))))

)

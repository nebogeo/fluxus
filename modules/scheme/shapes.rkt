;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc

(module fluxus racket

(require "fluxus-modules.ss")
(require "building-blocks.ss")
(require "maths.ss")
(require "randomness.ss")
(provide build-circle-points)

;; StartFunctionDoc-en
;; build-circle-points num-points radius
;; Returns: primitive-id
;; Description:
;; Returns a list of vectors describing a circle.
;; Useful for generating circles for the extrusion generator.
;; Example:
;; (clear)
;; (build-extrusion
;;     (build-circle-points 20 0.3)
;;     (list
;;         (vector 0 0 0)
;;         (vector 0 1 2)
;;         (vector 0 -1 4)
;;         (vector 0 0 6))
;;     (list 0 1 1 0) 1 (vector 0 1 0))
;; EndFunctionDoc

(define (build-circle-points n r)
    (define (_ n c l)
        (cond ((zero? c) l)
            (else
                (let ((a (* (/ c n) (* 2 3.141))))
                    (_ n (- c 1)
                        (cons (vmul (vector (sin a) (cos a) 0) r) l))))))
    (_ n n '()))

)

;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc

(module fluxus racket
(require scheme/class)
(require "fluxus.ss")

(provide
 expand
 cheap-toon
 occlusion-texture-bake)

;; StartFunctionDoc-en
;; pdata-for-each-tri-sample proc samples-per-triangle
;; Returns: void
;; Description:
;; Calls proc with the triangle indices and a random barycentric coord?
;; Example:
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; pdata-for-each-tri-sample proc amostras-por-triangulo
;; Retorna: void
;; Descrição:
;; Chama proc com os índices de triangulo e uma coordenada
;; baricentrica randomica?
;; Exemplo:
;; EndFunctionDoc

;; TODO: check if this info is correct please :) --greb

;; StartFunctionDoc-en
;; expand distance-value
;; Returns: void
;; Description:
;; Expand object along the normals
;; Example:
;; (with-primitive (build-cube)
;;   (expand 5))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; expand valor-distancia
;; Retorna: void
;; Descrição:
;; Expande o objeto em direção a suas normais.
;; Exemplo:
;; (with-primitive (build-cube)
;;   (expand 5))
;; EndFunctionDoc

(define (expand distance)
  (pdata-map!
   (lambda (p n)
     (vadd p (vmul n distance)))
   "p" "n"))

; apply a bargain basement toon outline effect
; attaches a copy of the object which is expanded and flipped inside out
; so area around edges becomes the unlit colour of the expanded object
(define (cheap-toon obj pen-width pen-colour)
  (with-state
    ; copy and parent a new object
    (parent obj)
          (with-primitive (with-primitive obj (build-copy obj))
                          ; setup toon appearance
                ;(poly-convert-to-indexed)
                          (hint-unlit)
                          (colour pen-colour)
                          ; grow and flip object inside out
                          (expand pen-width)
                          (hint-cull-ccw)))
  ;(with-primitive obj (recalc-normals 0))
  )




;; StartFunctionDoc-en
;; occlusion-texture-bake tex prim samples-per-face rays-per-sample ray-length debug
;; Returns: void
;; Description:
;; Bakes ambient occlusion textures. See ambient-occlusion.scm for more info.
;; Example:
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; occlusion-texture-bake tex prim amostras-por-face raios-por-amostra alcance-raio debug
;; Retorna: void
;; Descrição:
;; "Cozinha" texturas com ambient occlusion. Veja
;; ambient-occlusion.scm para mais informação.
;; Exemplo:
;; EndFunctionDoc

; proper ambient occlusion texture bake (very slow)
(define (occlusion-texture-bake tex prim samples-per-face rays-per-sample ray-length debug)

(define (make-sample-hemi n count l)
  (cond
    ((zero? count) l)
    (else
     (make-sample-hemi n (- count 1)
    (cons (vmul (hrndhemi n) ray-length) l)))))

  (define (sample-rnd-face-point indices b)
    (let
        ((pos (vadd
               (vadd (vmul (pdata-ref "p" (list-ref indices 0)) (vx b))
                     (vmul (pdata-ref "p" (list-ref indices 1)) (vy b)))
               (vmul (pdata-ref "p" (list-ref indices 2)) (vz b))))
         (norm (vadd
                (vadd (vmul (pdata-ref "n" (list-ref indices 0)) (vx b))
                      (vmul (pdata-ref "n" (list-ref indices 1)) (vy b)))
                (vmul (pdata-ref "n" (list-ref indices 2)) (vz b)))))
      (foldl
       (lambda (point r)

         (let ((a (vadd pos (vmul norm 0.04)))
               (b (vadd pos point)))

           ; visualise the rays
           #;(when debug
        (let ((l (with-state
                       (concat (with-primitive prim (get-transform)))
                       (hint-none)
                       (hint-unlit)
                       (hint-wire)
                       (hint-vertcols)
                       (wire-opacity 0.5)
                       (build-ribbon 2))))
               (with-primitive l
                               (pdata-set! "p" 0 a)
                               (pdata-set! "p" 1 b)
                 (pdata-set! "c" 0 (vector 0 0 0))
                               (pdata-set! "c" 1 (vector 1 1 1)))))

           (if (not (null? (geo/line-intersect a b)))
               (* r 0.95)
               r)))
       1
       (make-sample-hemi (vector 0 1 0) rays-per-sample '()))))

  (let ((w (with-primitive tex (pixels-width)))
        (h (with-primitive tex (pixels-height))))
    (with-primitive prim
                    (texture (pixels->texture tex))
                    (poly-for-each-tri-sample
                     (lambda (indices bary)
                       (let* ((v (sample-rnd-face-point indices bary))
                             (tc (vadd
                                  (vadd (vmul (pdata-ref "t" (list-ref indices 0)) (vx bary))
                                        (vmul (pdata-ref "t" (list-ref indices 1)) (vy bary)))
                                  (vmul (pdata-ref "t" (list-ref indices 2)) (vz bary))))
                             (tu (inexact->exact (round (* (vector-ref tc 0) w))))
                             (tv (inexact->exact (round (* (vector-ref tc 1) h)))))

                         (printf "sample: ~a ~n" v)

                         (with-primitive tex
                                         (when (< v 1)
                                           (pdata-set! "c" (+ tu (* tv w)) v))
                                         (pixels-upload))))
                     samples-per-face))))
)

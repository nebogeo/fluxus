; load a model from a obj file
; models have to be exported as triangulated

(clear)

(define obj (obj-import "bot.obj"))

; fix the normals
(grab obj)
(recalc-normals 0)
(ungrab)

; todos
; * particle sys lifetime management thing

;-------------------------------------------------------------------
; pop-get-verts - pulls the vertex data into a list, as a base for 
;                 deforming later

(define (pop-get-verts)
    (pop-get-verts-walk '() (pdata-size)))

(define (pop-get-verts-walk vertlist count)
    (set! vertlist (cons (pdata-get "p" count) vertlist))
    (if (< count 0)
        vertlist
        (pop-get-verts-walk vertlist (- count 1))))


;--------------------------------------------------------------------
; pop-deform - a general purpose deformer, takes the original 
;              reference geometry and a deformer function (see below)

(define (pop-deform vertlist deform-func)
    (pop-deform-walk (pdata-size) vertlist deform-func))

(define (pop-deform-walk count vertlist deform-func)
    (pdata-set "p" count (vadd (car vertlist) (deform-func (car vertlist))))
    (if (< count 0)
        0
        (pop-deform-walk (- count 1) (cdr vertlist) deform-func)))    
 
;--------------------------------------------------------------------
; deformers - deformer functions take a point and output the deformed 
;             point
 
(define (pop-wave-deformer p)
    (vector (* 0.1 (sin (+ (time) (*(vector-ref p 1) 10.4)))) 0 0))

;--------------------------------------------------------------------
;--------------------------------------------------------------------
; pop-shade - allows you to change 

(define (pop-shade shader-func)
    (pop-shade-walk (pdata-size) 
        (vtransform (vector 0 0 0) (get-camera-transform))
        (vtransform (vector 0 0 0) (get-transform))
        shader-func))

(define (pop-shade-walk count camerapos obpos shader-func)
    (shader-func count camerapos obpos)
    (if (< count 0)
        0
        (pop-shade-walk (- count 1) camerapos obpos shader-func)))    

;--------------------------------------------------------------------
; shaders are called once per vertex

(define (pop-toon-shader n camerapos obpos)
    (set! v (vadd obpos (pdata-get "p" n))) ; vertex in worldspace 
    (set! i (vnormalise (vsub v camerapos))) ; incident direction    
    (pdata-set "t" n (vector (vdot i (pdata-get "n" n)) 0 0))) ; set s to the facing ratio    


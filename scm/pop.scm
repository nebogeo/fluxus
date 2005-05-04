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
	(pop-deform-walk (pdata-size) vertlist deformer-func))

(define (pop-deform-walk count vertlist deformer-func)
    (pdata-set "p" count (vadd (car vertlist) (deform-func (car vertlist))))
    (if (< count 0)
        0
        (pop-deform-walk (- count 1) (cdr vertlist))))    
 
;--------------------------------------------------------------------
; deformers - deformer functions take a point and output the deformed 
;             point
 
(define (pop-wave-deformer p)
	(vector (* 0.1 (sin (+ (*(frame)0.01) (*(vector-ref p 1) 10.4)))) 0 0))




(clear)
(define v (vector 0 0 0))
(define t (vector 0 0 0))
(define points '())
(define ob (build-nurbs-sphere 10 20))

; reads the points into a list
(define (store n)
    (set! points (cons (pdata-get "p" n) points))
    (if (< n 0)
        0
        (store (- n 1)))) 

(define (deform n p)
    (set! v (vector (* 1 (sin (+ (*(frame)0.01) (*(vector-ref (car p) 1) 5.4)))) 0 0))
    (pdata-set "p" n (vadd v (car p)))
    (if (< n 0)
        0
        (deform (- n 1) (cdr p))))    

(define (render)
    (grab ob)
    (deform (pdata-size) points)
    (recalc-normals)
    (ungrab))

(grab ob)
(store (pdata-size))
(ungrab)

(engine-callback "(render)")
    




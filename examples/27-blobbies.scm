; an animated blobby script
; blobbies are a type of implicit surface, where a 3d field is sampled
; and a surface is generated using a meshing procedure called the 
; marching cubes algorithm

(clear)

;(hint-wire)
(hint-vertcols)
(shinyness 50)
(specular (vector 0.5 0.5 0.5))
(colour (vector 0 0.5 0))
(clear-colour (vector 0.5 0.5 0.5))

(show-fps 1)

(define a (build-blobby 7 (vector 30 30 30) (vector 3 3 3)))

(define (setup n)
    (define (loop n angle)
        (pdata-set "p" n (vadd (vector 1.5 1.5 1) 
            (vector (sin (* angle n)) (cos (* angle n)) 0 )))
        (pdata-set "c" n (vmul (vector (flxrnd) (flxrnd) (flxrnd)) 0.1))
        (pdata-set "s" n (* 0.04 (+ 1 n)))
        (if (zero? n)
            0
            (loop (- n 1) angle)))
    (loop n (/ 6.6 n)))
    

(define (animate n)
    (pdata-set "p" n (vadd (pdata-get "p" n) 
            (vector 0 0 (* (sin (+ n (time))) 0.03))))
    (if (zero? n)
        0
        (animate (- n 1))))
    

(grab a)
(setup (pdata-size))
(ungrab)


(define (render)
    (grab a)
    (animate (pdata-size))
    (ungrab))

(every-frame (render))
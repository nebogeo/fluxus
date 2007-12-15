; an animated blobby script
; blobbies are a type of implicit surface, where a 3d field is sampled
; and a surface is generated using a meshing procedure called the 
; marching cubes algorithm

(clear)
(clear-colour (vector 0.5 0.5 0.5))
(show-fps 1)

(define blobby (with-state
    ;(hint-wire)
    (hint-vertcols)
    (shinyness 50)
    (specular (vector 0.5 0.5 0.5))
    (colour (vector 0 0.5 0))    
    (build-blobby 7 (vector 30 30 30) (vector 3 3 3))))

(with-primitive blobby
    (let ((angle (/ 6.6 (pdata-size))))
    (pdata-index-map! 
        (lambda (i p) 
            (vadd (vector 1.5 1.5 1)
                (vector (sin (* angle i)) (cos (* angle i)) 0 )))
        "p")
    (pdata-map! 
        (lambda (c) (vmul (vector (flxrnd)(flxrnd)(flxrnd)) 0.1)) "c")
    (pdata-index-map! (lambda (i s) (* 0.04 (+ 1 i))) "s")))


(define (render)
    (with-primitive blobby
        (pdata-index-map!
            (lambda (i p)
                (vadd p (vector 0 0 (* (sin (+ i (time))) 0.03))))
            "p")))

(every-frame (render))
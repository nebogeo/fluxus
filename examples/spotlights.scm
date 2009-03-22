; this example sets up a scene with an attempt at proper lighting
; and some instances of a deformed sphere

(clear)

; light zero is the default camera light - set to a low level
(light-diffuse 0 (vector 0 0 0))
(light-specular 0 (vector 0 0 0))

; make a big fat key light
(define key (make-light 'spot 'free))
(light-position key (vector 5 5 0))
(light-diffuse key (vector 1 0.95 0.8))
(light-specular key (vector 0.6 0.3 0.1))
(light-spot-angle key 22)
(light-spot-exponent key 100)
(light-direction key (vector -1 -1 0))

; make a fill light
(define fill (make-light 'spot 'free))
(light-position fill (vector -7 7 12))
(light-diffuse fill (vector 0.5 0.3 0.1))
(light-specular fill (vector 0.5 0.3 0.05))
(light-spot-angle fill 12)
(light-spot-exponent fill 100)
(light-direction fill (vector 0.6 -0.6 -1))

; make a rim light
(define rim (make-light 'spot 'free))
(light-position rim (vector 0.5 7 -12))
(light-diffuse rim (vector 0 0.3 0.5))
(light-specular rim (vector 0.4 0.6 1))
(light-spot-angle rim 12)
(light-spot-exponent rim 100)
(light-direction rim (vector 0 -0.6 1))


(specular (vector 1 1 1))
(shinyness 80)

(with-state
    (rotate (vector 90 180 0))
    (scale (vector 10 10 10))
    (build-seg-plane 100 100))

; build and deform a sphere
(define (blob x y)
    (with-state
        (translate (vector 0 1 0))
        (let ((s (build-sphere 40 40)))
        (with-primitive s
            ; just a wacky deformation, takes two values
            ; for the deformation in different axes
            (pdata-index-map!
                (lambda (i p n)                    
                    (vadd p (vmul n (* 0.1 (+ (sin (* y 
                        (+ (sin (vector-ref p 0)) 
                           (cos (vector-ref p 2))))))
                        (sin (* (vector-ref p 1) x))))))
                "p" "n")                    
            (recalc-normals 1)
            s))))

; render instances of the blob in a circle
(define (copy n i)
    (define (loop a n)
        (cond ((not (zero? n))
            (with-state
                (rotate (vector 0 (* a n) 0))
                (translate (vector 3 1 0))
                (build-copy i))
                (loop a (- n 1)))))
    (loop (/ 360 n) n))

; stick an undeformed sphere in the centre (helps figure out lighting)
(with-state
    (scale (vector 0.5 0.5 0.5))
    (with-state 
        (translate (vector 0 0.5 0))
        (build-sphere 14 14))

    ; build the actual blob
    (let ((b (blob 10 8)))
        ; hide it (we only want the see the instances of it)
        (copy 10 b)
        (with-primitive b (hide 1))))

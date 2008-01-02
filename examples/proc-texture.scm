(clear)

(with-state

	;; simple random noise
    (with-primitive (build-pixels 100 100)
        (pdata-map! 
            (lambda (colour)
                (vector (flxrnd) 0 0 1))
            "c")
        (pixels-upload))
    
    (translate (vector 1.2 0 0))
    
	;; colourful stripes
    (with-primitive (build-pixels 100 100)
        (pdata-index-map! 
            (lambda (index colour)
                (vector (/ (modulo index 10) 10)
                        (/ (modulo index 50) 50) 
                        (/ (modulo index 20) 20) 1))
            "c")
        (pixels-upload))
    
    (translate (vector 1.2 0 0))
    
	;; b/w stripes
    (with-primitive (build-pixels 100 100)
        (pdata-index-map! 
            (lambda (index colour)
                (let ((v (floor (/ (modulo index 20) 10))))
                    (vector v v v 1)))
            "c")
        (pixels-upload))
)

(translate (vector 0 1.2 0))

(with-state

	;; colourful circles
    (with-primitive (build-pixels 100 100)
        (pdata-index-map! 
            (lambda (index colour)
                (let* ((x (quotient index 100)) 
                       (y (remainder index 100))
                       (d (vdist (vector 50 50 0) (vector x y 0))))
                    (vector (sin d) (cos d) 1)))
            "c")
        (pixels-upload))


    (translate (vector 1.2 0 0))

	;; b/w circles
    (with-primitive (build-pixels 100 100)
        (pdata-index-map! 
            (lambda (index colour)
                (let* ((x (quotient index 100)) 
                       (y (remainder index 100))
                       (d (vdist (vector 50 50 0) (vector x y 0)))
                       (v (floor (* 2 (sin (* d 0.3))))))
                    (vector v v v)))
            "c")
        (pixels-upload))


    (translate (vector 1.2 0 0))

	;; psychedelic mess
    (with-primitive (build-pixels 100 100)
        (pdata-index-map! 
            (lambda (index colour)
                (let* ((x (quotient index 100)) 
                       (y (remainder index 100))
                       (d (vdist (vector 50 50 0) (vector x y 0)))
                       (r (* (sin d) (cos (* y 0.5))))
                       (g (* (sin d) (cos (* x 0.3))))
                       (b (* (sin d) (cos (* y 0.2)))))

                    (vector r g b)))
            "c")
        (pixels-upload))


)

(clear)

(define (plot fn str)
    (with-state
        (translate (vector -1 -2 0))
        (scale 0.1)
        (hint-unlit)
        (build-type "Bitstream-Vera-Sans-Mono.ttf" str))
    
    (let ((p (with-state    
                    (hint-none)
                    (hint-points)
                    (hint-origin)
                    (point-width 1)
                    (build-particles 1000))))
        
        (with-primitive p
            (pdata-map! 
                (lambda (p)
                    (vector 1 1 1))
                "c")
            (pdata-map! 
                (lambda (p)
                    (fn))
                "p"))))

(with-state
    (plot rndvec "(rndvec)")
    (translate (vector 2.5 0 0))
    (plot crndvec "(crndvec)")
    (translate (vector 2.5 0 0))
    (plot srndvec "(srndvec)")
    (translate (vector 2.5 0 0))
    (plot hsrndvec "(hsrndvec)")
    (translate (vector -4 -5 0))
    (plot grndvec "(grndvec)"))

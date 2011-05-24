; an example of using fluxus's audio synth.
; make sure fluxa is running and connected 
; to jack [run 'fluxa' on the command line].

(require fluxus-018/fluxa)

(seq  
    (lambda (time clock)

            (play time (mul (pink 2) (adsr 0 0.02 0.1 0.3)))

            (when (or (zmod clock 4) (zmod clock 7))
                  (play time (mul 
                    (moogbp (white 4) (pow (adsr 0 (rndf) 0 0) 6) 0.2)
                         (adsr 0 0.1 0.1 0))))

            (when (zmod (+ clock 2) 4)
                  (play time (mul (sine (mul 100 (pow (adsr 0 0.1 0.8 1) 6)))
                      (adsr 0 0.5 0.4 5))))

            (when (zmod (+ clock 2) 8)
                  (play time (crush (mul (sine (mul 600 (adsr 0 0.1 0.1 2)))
                      (adsr 0 0.1 0.4 8)) (random 5) 0.5)))
  
            (clock-map
                (lambda (n)
                        (play time 
                            (mul
                            (mooghp (add 
                                (saw (note (+ n 5)))
                                (saw (note n))) 
                              (mul (sine 0.6) 0.1) 0.13)
                             (adsr 0 0.05 0.05 3))))
                clock
                (list (+ 4 (* 2 (modulo clock 12)))
                      (+ 6 (* 4 (modulo clock 12)))
                      (+ 8 (* 3 (modulo clock 14)))))
        0.2))

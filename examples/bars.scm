; draw a row of cubes, scaled 
; by the harmonics from the sound input

(define num-bars 16)

(define (bars c)
    (cond ((not (negative? c))
        (translate (vector 1.1 0 0))
        (with-state
            (colour (vector 1 0 (gh c)))
            (scale (vector 1 (+ 0.1 (* 5 (gh c))) 1))
            (draw-cube))
        (bars (- c 1)))))

(start-audio "alsa_pcm:capture_1" 1024 44100)
(set-num-frequency-bins num-bars)
(every-frame (bars (- num-bars 1)))

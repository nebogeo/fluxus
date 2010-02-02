; draw a row of cubes, scaled 
; by the harmonics from the sound input

(define (bars c)
    (cond ((not (zero? c))
        (translate (vector 1.1 0 0))
        (with-state
            (colour (vector 1 0 (gh c)))
            (scale (vector 1 (+ 0.1 (* 5 (gh c))) 1))
            (draw-cube))
        (bars (- c 1)))))

(start-audio "alsa_pcm:capture_1" 512 44100)
(every-frame (bars 16))

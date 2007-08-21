; draw a row of cubes, scaled 
; by the harmonics from the sound

(define (bars c)
    (cond 
        ((zero? c) 0)
        (else
            (translate (vector 1 0 0))
            (push)
            (colour (vector 1 0 (gh c)))
            (scale (vector 1 (+ 0.1 (* 5 (gh c))) 1))
            (draw-cube)
            (pop)
            (bars (- c 1)))))

(define (render)
    (bars 16))

(start-audio "alsa_pcm:capture_1" 1024 44100)
(every-frame (render))

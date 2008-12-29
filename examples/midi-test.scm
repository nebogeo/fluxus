; simple midi test
; a cube is scaled by the value of controllers 1, 2, 3 on channel 0 

(require fluxus-016/fluxus-midi)

(display (midi-info))(newline)

(midi-init 1)

(define (midi-test)
	(with-state
		(scale (vector (midi-ccn 0 1) (midi-ccn 0 2) (midi-ccn 0 3)))
		(draw-cube)))

(every-frame (midi-test))



;; a simple example of using data sent via osc to re-scales a cube
;; based on /x, /y and /z messages. 

(define x 1)
(define y 1)
(define z 1)

(define (osc-test)
  ; (display (osc-peek)) (newline)
  (if (osc-msg "/x")
      (begin
	(set! x (osc 0))
	(display "x: ") (display x) (newline)))   
  (if (osc-msg "/y")
      (begin
	(set! x (osc 0))
	(display "y: ") (display y) (newline)))
  (if (osc-msg "/z")
      (begin
	(set! z (osc 0))
	(display "z: ") (display z) (newline)))   

  (push)
  (scale (vector x y z))
  (draw-cube)
  (pop))


(every-frame "(osc-test)")

;; #<unspecified>


(define (osctest)
    (display (from-osc "/hellofluxus"))
    (newline))

(start-osc "88000")

(engine-callback "(osctest)")

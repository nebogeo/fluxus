
(define (osctest)
    (osc-send "/hello?" "ifs" (list 23 40.3 "fluxus"))
    (display (osc-peek))(newline))

(osc-source "4444")
(osc-destination "osc.udp://localhost:4444")

(every-frame (osctest))

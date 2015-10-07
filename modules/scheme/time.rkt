;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

(module fluxus racket

(provide
 time->timestamp
 timestamp->time
 time-now)

(define (time-now)
  (/ (current-inexact-milliseconds) 1000))

; converts from UTC time to get a 64bit NTP timestamp
(define (time->timestamp time)
  ; january 1972 UTC -> january 1900 NTP era (overflow in 2036...)
  (let ((adjusted (+ time 2208988800L0)))
    ; floor the time for the seconds
    (let ((seconds (inexact->exact (floor adjusted))))
      ; get the remainder and scale to max unsigned int for the fraction of the second
      (let ((frac (inexact->exact (floor (* (- adjusted seconds) 4294967295)))))
        (vector seconds frac)))))

; ... and back the other way
(define (timestamp->time timestamp)
  (+ (- (vector-ref timestamp 0) 2208988800L0) (/ (vector-ref timestamp 1) 4294967295.0)))

)

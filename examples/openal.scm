; a stupid open al example
(oa-start)
(define s (oa-load-sample (fullpath "sample.wav")))
(oa-play s (vector 0 0 0) 1 0.3)

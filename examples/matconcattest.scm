

(clear)
(define m (mident))
(set! m (mmul (mscale (vector 0.5 1 1)) m))
(push)
(concat m)
(build-sphere 10 20)
(pop) 
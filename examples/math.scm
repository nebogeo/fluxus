

(define m (mident))

(define test (vector 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1))
(display test) (newline)


(set! m (mmul test test))
(display m) (newline)

(set! m (mmul m (mrotate (vector 45 0 0))))
(display m) (newline)


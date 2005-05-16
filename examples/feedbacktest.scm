
(clear)

(push)
(colour (vector 1 1 1))
(define a (build-cube))
(pop)
(show-axis 1)

(feedback 1)
(feedback-transform (mmul (mscale (vector 1.1 1.1 1)) (mrotate (vector 0 0 10))))


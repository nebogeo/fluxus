; how to set up:
; * a contained physics system
; * mouse interactivity
; * feedback from the physics system

(clear)

(define (build-world size)
    (ground-plane (vector 0  1 0) (- size))
    (ground-plane (vector 0 -1 0) (- size))
    (ground-plane (vector -1 0 0) (- size))
    (ground-plane (vector 1  0 0) (- size))    
    (ground-plane (vector 0  0 1) (- size))
    (ground-plane (vector 0 0 -1) (- size))

    (push)
    (hint-none)
    (hint-wire)
    (scale (vmul (vector size size size) 2))
    (grab (build-cube))
    (selectable 0)
    (ungrab)
    (pop))

(define (add-sphere)
    (push)
    (scale (vector 0.75 0.75 0.75))
    (set! obs (cons (build-sphere 8 10) obs))
    (active-sphere (car obs))
    (pop))

(define (add-cube)
    (push)
    (hint-wire)
    (set! obs (cons (build-cube) obs))
    (active-box (car obs))
    (pop))

(define (update l)
    (grab (car l))
    (if (has-collided (car l))
        (colour (vector 1 0 0))
        (colour (vector 1 1 1)))
    (ungrab)
    (if (eq? (cdr l) '())
        0
        (update (cdr l))))

(define (render)
    (update obs)
    (if (or (mouse-button 1) (mouse-button 3))
        (let ((selected (mouse-over)))
            (if (> selected 0)
                (begin
                    (cond 
                        ((mouse-button 1)
                            (grab selected)
                            (colour (vector 0 0.5 1))
                            (ungrab)
                            (kick selected (vector 0 1 0)))
                        ((mouse-button 3)
                            (grab selected)
                            (colour (vector 1 1 1))
                            (ungrab))))))))


(define obs '())
(clear-colour (vector 0 0.8 0.5))
(backfacecull 0)
(line-width 3)

(collisions 1)
(surface-params 0.4 0 3 5)
(gravity (vector 0 0 0))

(build-world 3)
(add-sphere)
(add-sphere)
(add-sphere)
(add-cube)
(add-cube)
(add-cube)

(every-frame "(render)")








 

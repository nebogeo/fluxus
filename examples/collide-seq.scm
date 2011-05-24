; click on the objects!
; same as mouse-interactive, but with sound!
; make sure you're also running jackd and fluxa

(require fluxus-018/fluxa)

(clear)

(wire-colour (vector 0 0 0))

(define (build-world size)
    (ground-plane (vector 0  1 0) (- size))
    (ground-plane (vector 0 -1 0) (- size))
    (ground-plane (vector -1 0 0) (- size))
    (ground-plane (vector 1  0 0) (- size))    
    (ground-plane (vector 0  0 1) (- size))
    (ground-plane (vector 0 0 -1) (- size))

    (with-state
        (hint-none)
        (hint-wire)
        (scale (vmul (vector size size size) 2))
        (with-primitive (build-cube) (selectable 0))))
    
(define (make-sphere)
    (with-state
        (scale (vector 0.75 0.75 0.75))
        (let ((obj (build-sphere 8 10)))
            (active-sphere obj)
            obj)))

(define (make-cube)
    (with-state
        (hint-wire)
        (let ((obj (build-cube)))
            (active-box obj)
            obj)))

(define (render obs)
    ; change colour if they have collided
    (for-each
        (lambda (obj)
            (with-primitive obj
                (cond ((has-collided obj)
                    (play-now (mul (sine (note (* obj 10))) (adsr 0 0.1 0 0)))
                    (colour (vector 1 0 0)))
                    (else
                    (colour (vector 1 1 1))))))
        obs)
            
    ; look for mouse clicks
    (when (mouse-button 1)
        (let ((selected (mouse-over)))
            (cond ((not (zero? selected))
                (with-primitive selected 
                    (colour (vector 0 0.5 1)))
                    (kick selected (vector 0 1 0)))))))


(clear-colour (vector 0 0.8 0.5))
(backfacecull 0)
(line-width 3)

(collisions 1)
(surface-params 0.4 0 3 5)
(gravity (vector 0 0 0))

(build-world 3)
(define obs (list (make-sphere)
                  (make-sphere)
                  (make-sphere)
                  (make-cube)
                  (make-cube)
                  (make-cube)))

(desiredfps 1000)
(every-frame (render obs))








 

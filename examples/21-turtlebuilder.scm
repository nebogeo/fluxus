(start-audio "alsa_pcm:capture_1" 1024 44100)

(define pi 3.141593)
(define glob-list '())

(define (build-glob-list n)
    (set! glob-list (cons 0 glob-list))
    (if (< n 1)
        0
        (build-glob-list (- n 1))))

(build-glob-list 1000)

(define (shape n numsides numpetals petalsize)
    (turtle-vert)
    (turtle-move (gh 13))

    (let ((mod (* (sin (/ n (/ (* 2 pi) numpetals))) petalsize)))
        (turtle-turn (vector 0 (+ (/ 360.0 numsides) mod) 0)))
    
    (if (< n 0)
        1
        (shape (- n 1) numsides numpetals petalsize)))


(define (build-flower numpetals petalsize)
    (turtle-reset)
    (turtle-turn (vector -90 0 0))
    (turtle-prim 4)
    (shape 20 20 numpetals petalsize)
    (turtle-build))

(define (flowers n)
    (translate (vector 0 0 (flxrnd)))
    (push)
    (colour (vmul (vector (gh 3) (gh 4) (gh 5)) 0.1))
    (let ((flower (build-flower (+ 2 (* (gh 8) 100)) (* 20 (gh 9)))))
        (active-sphere flower)

        (kick flower (vmul (vector (sin (gh 4)) (cos (gh 4)) 0) 5))
        (twist flower (vmul (vector 0 0 (* 2 (gh 6))) 0.01))

        (set! glob-list (reverse glob-list))
        (destroy (car glob-list))
        (set! glob-list (cdr glob-list))
        (set! glob-list (reverse glob-list))
        (set! glob-list (cons flower glob-list)))
        
    (pop)
    (if (eq? n 0)
        1
        (flowers (- n 1))))
    
(backfacecull 0)    
(clear)
(collisions 0)
(hint-unlit)
(hint-wire)
(line-width 8)
(clear-colour (vector 1 1 1))

(desiredfps 250000)

(gravity (vector 0 0 0))

(define (render)
    (if (> (gh 1) 0.1)
        (flowers 0)))

(blur 0)
(gain 10)

(every-frame (render))

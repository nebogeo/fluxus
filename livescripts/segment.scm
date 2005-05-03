
(define objs `())
(define joints `())
(define segments '())

(define (buildleg)
    (push)
    (scale (vector 4.5 0.4 0.4))
    (set! objs (cons (build-cube) objs))
    (active-box (car objs))
    (pop)

    (push)
    (translate (vector 5 0 0))
    (scale (vector 4.5 0.4 0.4))
    (set! objs (cons (build-cube) objs))
    (active-box (car objs))
    (pop))

(define (initjoint j)
    (joint-param j "LoStop" -2) 
    (joint-param j "HiStop" 2)     
    (joint-param j "FMax" 10) 
    (joint-param j "FudgeFactor" 0))
    
(define (connectleg pos vel hinge)

    (set! joints (cons (build-hingejoint (car objs) 
        (car (cdr objs)) pos hinge) joints))

    (initjoint (car joints))
    ;(joint-param (car joints) "Vel" vel)
    )

(define (build-segment z)
    (push)
    (translate (vector 0 0 z))

    (push)
    (scale (vector 5 2 1))
    (set! segments (cons (build-cube) segments))
    (active-box (car segments))
    (pop)

    (push)    
    (translate (vector 5 0 0))
    (buildleg)
    (connectleg (vector 7.5 0 z) -1 (vector 0 0 1))
    (pop)

    (set! joints (cons (build-hingejoint (car (cdr objs)) 
        (car segments) (vector 2.5 0 z) (vector 0 0 1)) joints))
    (initjoint (car joints))

    (push)   
    (rotate (vector 0 180 0)) 
    (translate (vector 5 0 0))
    (buildleg)
    (connectleg (vector -7.5 0 z) 1 (vector 0 0 -1))
    (pop)

    (set! joints (cons (build-hingejoint (car (cdr objs)) 
        (car segments) (vector -2.5 0 z) (vector 0 0 -1)) joints))
    (initjoint (car joints))

    (pop)
    (car segments))

(define (build n)
    (build-segment 0)
    (build-segment 2)
    (set! joints (cons (build-hingejoint (car (cdr segments)) 
        (car segments) (vector -2.5 0 1) (vector 0 1 0)) joints))
    (initjoint (car joints))
    (build-segment 4)
    (set! joints (cons (build-hingejoint (car (cdr segments)) 
        (car segments) (vector -2.5 0 3) (vector 0 1 0)) joints))
    (initjoint (car joints))



    )

(define (animate jl n)
    ;(joint-param (car jl) "Vel" (* 50 (- 0.5 (gh n))))
    (joint-angle (car jl) 5 (* -1 (gh n)))
    (if (eq? (cdr jl) '())
        '()
        (animate (cdr jl) (+ n 1))))
    
(clear)
(collisions 1)
(gravity (vector 0 -1 0))
(define l '())
(show-axis 1)

(build 2)

(desiredfps 1000)
(ground-plane (vector 0 1 0) -1)

(push)
    (colour (vector 0 0.5 0))
    (translate (vector 0 -1 0))
    (rotate (vector 90 0 0))
    (scale (vector 30 30 30))
    (build-plane)
(pop)

(define (update)
    (animate joints 0))

(engine-callback "(update)")






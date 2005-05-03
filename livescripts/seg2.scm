
(define objs `())
(define joints `())
(define segjoints `())
(define segments `())
(define num-segments 16)

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
    (joint-param j "FMax" 50) 
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

(define (build)
    (build-segment 0)
    (build-sub 2))

(define (build-sub n)
    (build-segment n)    
    (set! segjoints (cons (build-hingejoint (car (cdr segments)) 
        (car segments) (vector 0 0 (- n 1)) (vector 0 1 0)) segjoints))
    (initjoint (car segjoints))

    (if (> n num-segments)
        0
        (build-sub (+ 2 n))))

(define (animatelegs jl n)
    (joint-angle (car jl) 20 (* -1 (gh n)))
    (if (eq? (cdr jl) '())
        '()
        (animatelegs (cdr jl) (+ n 1))))

(define (animatesegs jl n)
    (joint-angle (car jl) 20 (- (gh n) 0.5))
    (if (eq? (cdr jl) '())
        '()
        (animatesegs (cdr jl) (+ n 1))))
    
(clear)
(collisions 1)
(gravity (vector 0 -1 0))
(define l '())
(show-axis 0)

(colour (vector 1 0 0))
(build)

(desiredfps 1000)
(ground-plane (vector 0 1 0) -1)

(push)
    (colour (vector 0 1 0))
    (translate (vector 0 -1 0))
    (rotate (vector 90 0 0))
    (scale (vector 300 300 1))
    ;(build-plane)
(pop)

(define (update)
    (animatelegs joints 0)
    (animatesegs segjoints 0))

(clear-colour (vector 0 0 1))

(engine-callback "(update)")






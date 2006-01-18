; an example of a simple driving game
; not much explanation here, but the majority of the code is involved
; with connecting the various bits of the car together

(clear)
(colour (vector 1 1 1))
(collisions 1)
(define speed 0)
(define steer 0)

(define wheel-rad 0.5)
(define length 2)
(define width 1)
(define height 0.2)

(define wheel-mass 0.1)
(gravity (vector 0 -2 0))
(surface-params 0.1 0.1 0.5 0.3)

(push)
    (define (build-wheel)
        (colour (vector 0.4 0.2 0.0))
        (scale (vector wheel-rad wheel-rad wheel-rad))
        (rotate (vector 90 0 0))
        (rotate (vector 0 0 90))
        (translate (vector 0 -0.5 0))
        ;(hint-none)
        ;(hint-wire)
        (build-cylinder 1 10))
        
    (translate (vector 0 2 0))
  
    (push)
        (colour (vector 0.2 0.2 0.2))
        (scale (vector width height length))
        (define car-body (build-cube))
        (active-box car-body)
        ;(set-mass car-body 0.1)
    (pop)   
    (push)
        (translate (vector 1.1 0 1.5))
        (define back-right-wheel (build-wheel))
        (active-sphere back-right-wheel)
           ;(set-mass back-right-wheel wheel-mass)
    (pop)
    (push)
        (translate (vector -1.1 0 1.5))
        (define back-left-wheel (build-wheel))
        (active-sphere back-left-wheel)
        ;(set-mass back-left-wheel wheel-mass)
    (pop)
    (push)
        (translate (vector 0 -1 -1.5))
        (define front-right-wheel (build-wheel))
        (active-sphere front-right-wheel)
        ;(set-mass front-right-wheel wheel-mass)
    (pop)
    ;(push)
    ;    (translate (vector -1.1 -1 -1.5))
    ;    (define front-left-wheel (build-wheel))
    ;    (active-sphere front-left-wheel)
    ;    (set-mass front-left-wheel wheel-mass)
    ;(pop)
    
    (define back-right-joint (build-hinge2joint car-body back-right-wheel (vector 1.1 2 1.5) (vector 0 1 0) (vector 1 0 0)))
    (define back-left-joint (build-hinge2joint car-body back-left-wheel (vector -1.1 2 1.5) (vector 0 1 0) (vector 1 0 0)))
    (define front-right-joint (build-hinge2joint car-body front-right-wheel (vector 0 1 -1.5) (vector 0 1 0) (vector 1 0 0)))
    ;(define front-left-joint (build-hinge2joint car-body front-left-wheel (vector -1.1 1 -1.5) (vector 0 1 0) (vector 1 0 0)))
    
    (joint-param back-left-joint "SuspensionERP" 0.4)       
    (joint-param back-left-joint "SuspensionCFM" 0.8)       
    (joint-param back-right-joint "SuspensionERP" 0.4)       
    (joint-param back-right-joint "SuspensionCFM" 0.8)           
    (joint-param front-right-joint "SuspensionERP" 0.4)       
    (joint-param front-right-joint "SuspensionCFM" 0.8)       
     
    (joint-param back-left-joint "LoStop" 0)       
    (joint-param back-right-joint "HiStop" 0)
    (joint-param back-right-joint "LoStop" 0)
    (joint-param back-left-joint "HiStop" 0)    

(pop)

(define (update)
    
    (joint-param front-right-joint "Vel2" speed)
    (joint-param front-right-joint "FMax2" 0.1)
    
    (joint-param front-right-joint "Vel" steer)
    (joint-param front-right-joint "FMax" 0.2)
    (joint-param front-right-joint "LoStop" -0.75)
    (joint-param front-right-joint "HiStop" 0.75)
    (joint-param front-right-joint "FudgeFactor" 0.1))
    
(define (rand-range min max)
    (+ min (* (flxrnd) (- max min))))
    
(define (build-terrain n)
    (push)
        (colour (vector (flxrnd) (flxrnd) (flxrnd)))
        (translate (vector (rand-range -60 60) 0 (rand-range -60 60)))
        ;(rotate (vector (rand-range 0 360) (rand-range 0 360) (rand-range 0 360)))
        (scale (vector (rand-range 1 5) (rand-range 1 5) (rand-range 1 5)))
        (active-box (build-cube))
    (pop)
    (if (zero? n)
        1
        (build-terrain (- n 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    

(push)
(texture (load-texture "textures/track.png"))
(hint-unlit)
(translate (vector 0 -0.5 0))
(scale (vector 100 1 100))
(build-cube)
(pop)
(ground-plane (vector 0 1 0) 0.5)

(lock-camera car-body)

(build-terrain 100)
;(blur 0.2)

(define (run-loop)
    (update)
    
    (if (key-pressed " ")
        (begin
        (set! speed 0)
        (set! steer 0)))
    
    (if (key-pressed "q")
        (set! speed (+ speed 0.3)))
    (if (key-pressed "a")
        (set! speed (- speed 0.3)))
    (if (key-pressed "o")
        (set! steer (- steer 0.1)))
    (if (key-pressed "p")
        (set! steer (+ steer 0.1)))
    
    (set! steer (* steer 0.9)))
    
(every-frame "(run-loop)")

; a flocking script. the flocking algorithm is a simplified 
; version of the craig reynolds boids one, with just two rules: 

; 1. move away from the nearest neighbor 
; 2. move towards the origin

; this script is set up to work in 2 dimensions, but it can easily 
; work in three (just modify the init function) - it just looks 
; better in 2 I think


; constants, tweak - or animate these to achieve different behavours
(define neighbor-avoidance 2.0)
(define home-attraction 0.4)
(define speed 0.001)
(define acceleration 0.01)

(define (rndvec)
    (vsub (vector 0 (flxrnd) (flxrnd)) 
        (vector 0 0.5 0.5)))

; the flocking function
(define (flock n)
    (cond ((not (zero? n))
        (let ((closest (pdata-op "closest" "p" n))) ; find the closest neighbor    
            ; find the direction the closest, and scale by the amount we want to avoid them
            (let ((dir (vmul (vnormalise (vsub (pdata-get "p" n) closest)) neighbor-avoidance))         
                  ; find the direction to the origin, and scale
                  (centre (vmul (vnormalise (vsub (vector 0 0 0) (pdata-get "p" n))) home-attraction)))
                  
                ; apply the result to our velocity, and scale by speed, mix in the previous            
                ; velocity, and scale that by acceleration, so we don't change direction too fast
                (pdata-set "vel" n 
                    (vmul (vnormalise 
                        (vadd (vmul (vadd dir centre) speed) (pdata-get "vel" n))) acceleration ))))
                (flock (- n 1)))))

(define (anim)
    (with-primitive particles
        (flock (pdata-size))
        ; add the velocity to the position in one command, animating the flock
        (pdata-op "+" "p" "vel")))

(clear)

(define particles (with-state
    (rotate (vector 0 90 0))
    (hint-none)
    (hint-points)
    (hint-anti-alias)
    (point-width 3)
    (build-particles 500)))


(with-primitive particles
    ; make the velocity pdata
    (pdata-add "vel" "v")
    ; setup each flock entity with a random velocity and colour
    (pdata-map! (lambda (vel) (rndvec)) "vel")
    (pdata-map! (lambda (c) (vector 1 1 1)) "c"))


(desiredfps 100000)
(blur 0.1)
(every-frame (anim))



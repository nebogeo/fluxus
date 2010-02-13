; dancing robots : an example of using structs to help build more 
; complex fluxus scripts

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; define the robot struct. this automatically creates the functions (make-robot)
; (robot-pos) (robot-vel) and (robot-root). if there is a mutable flag 
; it also creates set functions, i.e. (set-robot-pos!) and (set-robot-vel!)
(define-struct robot ((pos #:mutable) (vel #:mutable) root))

; make a function to build a robot given a position
(define (build-robot pos)
    (let ((root (with-state
                    (translate pos)
                    (colour (rndvec))
                    (load-primitive "bot.obj"))))
        (with-primitive root
            (recalc-normals 0)) ; the normals on bot.obj are wrong, so recalc them
        (make-robot pos (vmul (srndvec) 0.01) root)))


; this is the function which gets run every frame on a robot
; and does all the complex stuff
(define (robot-update robot world)

    ; loop over all the robots, so we can avoid them 
    (for-each
        (lambda (other-robot)
            (when (not (eq? robot other-robot)) ; don't want to avoid ourself!
                ; get the vector between the robot positions
                (let ((dir (vsub (robot-pos robot) (robot-pos other-robot))))
                     ; add a normalised, scaled version to our velocity. 
                     ; to move us away from this robot
                     (set-robot-vel! robot (vadd (robot-vel robot)
                        (vmul (vnormalise dir) 0.15))))))
        (world-robots world))

    ; set an attraction to the centre of the world (0 0 0)
    (set-robot-vel! robot (vadd (robot-vel robot)
        (vmul (robot-pos robot) -0.2)))

    ; clamp the speed
    (set-robot-vel! robot (vmul (vnormalise (robot-vel robot)) 0.1))
    ; get rid of the z component, so we dance on the floor (try commenting this out)
    (set-robot-vel! robot (vector (vx (robot-vel robot)) (vy (robot-vel robot)) 0))    

    ; add this final velocity to the position
    (set-robot-pos! robot (vadd (robot-vel robot) (robot-pos robot)))
 
    ; now we can move the primitive to the right place
    (with-primitive (robot-root robot)
        ; clear the last transform
        (identity)
        ; move to the current position
        (translate (robot-pos robot)) 
        ; aim to face in the right direction
        (concat (maim (vnormalise (robot-vel robot)) (vector 0 0 -1)))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; our world - just contains a list of robots
(define-struct world (robots))

; a builder for the world
(define (build-world robot-count)
    (make-world (build-list robot-count
        (lambda (_)
            ; build a robot at a random position on the floor
            (build-robot (vmul (vector (crndf) (crndf) 0) 10))))))

; update the world
(define (world-update world)
    ; loop over all the robots
    (for-each
        (lambda (robot)
            ; call update on the robot
            (robot-update robot world))
        (world-robots world)))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; build and run the world

(clear)
(define world (build-world 10))
(every-frame (world-update world))

; infinite asteroids
; GPL licence (c) dave griffiths 2006 
; shoot asteroids for eternity, or 0 fps!

; keys:
; q : pitch down
; a : pitch up
; o ; roll left
; p : roll right
; space bar : fire
; z : accel
; x : decel
; h : hyperspace

; ctrl-h to hide the code!

(desiredfps 1000)
(clear)
(blur 0)
(define current-bullet 0)
(hint-none)
(hint-wire)

(clip 1 100000)
(camera-lag 0.02)
(reset-camera)

(set-camera-transform (mmul (mtranslate (vector 0 -5 -10))
                            (mrotate (vector 0 90 0))))

(define WORLD_SIZE 2000)

(define (rndvec)
    (vadd (vector (flxrnd) (flxrnd) (flxrnd)) (vector -0.5 -0.5 -0.5)))

(define (make-ob ob) (list (vector 1 0 0) (vector 0 1 0) (vector 0 0 0) (vector 0 0 0) 0 ob))
(define (get-dir ob) (list-ref ob 0))
(define (set-dir ob dir) (list-set! ob 0 (vnormalise dir)))
(define (get-up ob) (list-ref ob 1))
(define (set-up ob dir) (list-set! ob 1 dir))
(define (get-pos ob) (list-ref ob 2))
(define (set-pos ob pos) (list-set! ob 2 pos))
(define (get-rotvel ob) (list-ref ob 3))
(define (set-rotvel ob rot) (list-set! ob 3 rot))
(define (get-speed ob) (list-ref ob 4))
(define (set-speed ob speed) (list-set! ob 4 speed))
(define (get-obj ob) (list-ref ob 5))

(define (update-ob ob)
    (set-dir ob (vtransform (get-dir ob) 
                (mrotate (qaxisangle (get-up ob) 
                (vector-ref (get-rotvel ob) 0))))) 
    (set-up ob (vtransform (get-up ob) 
                (mrotate (qaxisangle (get-dir ob) 
                (vector-ref (get-rotvel ob) 1))))) 

    ;(set-speed ob (* (get-speed ob) 0.998))
    (set-pos ob (vadd (get-pos ob) 
        (vmul (get-dir ob) (* (- (get-speed ob)) (delta)))))

    ; clamp pos
    (let ((pos (get-pos ob)))
    (cond ((< (vector-ref pos 0) (- WORLD_SIZE))
              (vector-set! pos 0 WORLD_SIZE))
          ((< (vector-ref pos 1) (- WORLD_SIZE))
              (vector-set! pos 1 WORLD_SIZE))
          ((< (vector-ref pos 2) (- WORLD_SIZE))
              (vector-set! pos 2 WORLD_SIZE))
          ((> (vector-ref pos 0) WORLD_SIZE)
              (vector-set! pos 0 (- WORLD_SIZE)))
          ((> (vector-ref pos 1) WORLD_SIZE)
              (vector-set! pos 1 (- WORLD_SIZE)))          
          ((> (vector-ref pos 2) WORLD_SIZE)
              (vector-set! pos 2 (- WORLD_SIZE))))
     (set-pos ob pos)))
    
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-bullets)
    (define (init-particles n)
        (pdata-set "s" n (vector 1 1 1))
        (pdata-set "p" n (vector 0 0 0))
        (let ((c (flxrnd))) 
        (pdata-set "c" n (vector c c c)))
        (pdata-set "vel" n (vector 0 1 0))
        (if (zero? n)
            0
            (init-particles (- n 1))))
    (let ((bullets (build-particles 100)))
    (grab bullets)
    (hint-solid)
    (pdata-add "vel" "v")
    (init-particles (pdata-size))
    (ungrab)
    bullets))

(define (bullets-update bullets)
    (grab bullets)
    (pdata-op "+" "p" "vel")
    (ungrab))
        
(define (bullet-fire bullets pos vec)
    (grab bullets)
    (pdata-set "p" current-bullet pos)
    (pdata-set "vel" current-bullet (vmul vec (delta)))
    (set! current-bullet (modulo (+ current-bullet 1) (pdata-size)))
    (ungrab))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-particle-system pos)
    (define (init-particles n pos)
        (pdata-set "s" n (vector 10 10 10))
        (pdata-set "p" n pos)
        (pdata-set "c" n (vector 1 1 1))
        (pdata-set "vel" n (vmul (rndvec) 10))
        (if (zero? n)
            0
            (init-particles (- n 1) pos)))
    (let ((p (build-particles 100)))
    (grab p)
    (hint-solid)
    (pdata-add "vel" "v")
    (init-particles (pdata-size) pos)
    (ungrab)
    p))

(define (particle-system-update ps)
    (grab ps)
    (pdata-op "+" "p" "vel")
    (pdata-op "*" "c" 0.99)
    (let ((age (pdata-get "c" 0)))
    (ungrab)
    (if (< (vector-ref age 0) 0.1)
        1
        0)))

(define (particle-system-add l pos)
    (set! l (cons (make-particle-system pos) l))
    l)

(define (particle-systems-update l)
    (cond ((not (null? l))
        (cond ((eqv? (particle-system-update (car l)) 1)
            (destroy (car l))
            (set! l (cdr l))))))
    (if (or (null? l) (null? (cdr l)))
        l
        (cons (car l) (particle-systems-update (cdr l)))))

(define particle-system '())

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-asteroid size pos) 
    (push)
    (wire-colour (vector 1 1 1))
    (scale (vector size size size))
    (let ((ob (build-sphere 4 5)))
    (pop)
    (let ((asteroid (append (make-ob ob) (list size))))
    (apply-transform ob)
    (set-pos asteroid pos)
    (set-dir asteroid (vmul (rndvec) 0.001))
    (set-rotvel asteroid (rndvec))
    (set-speed asteroid (* (flxrnd) 100))
    asteroid)))

(define (make-asteroid-child parent) 
    (push)
    (wire-colour (vector 1 1 1))
    (let ((size (* (asteroid-get-size parent) 0.5)))
    (scale (vector size size size))
    (let ((ob (build-sphere 4 5)))
    (pop)
    (let ((asteroid (append (make-ob ob) (list size))))
    (apply-transform ob)
    (set-pos asteroid (vadd (get-pos parent) (vmul (rndvec) size)))
    (set-dir asteroid (rndvec))
    (set-rotvel asteroid (get-rotvel parent))
    (set-speed asteroid (* (get-speed parent) 2))
    asteroid))))
    
(define (asteroid-get-size asteroid)
    (list-ref asteroid 6))

(define (asteroid-update asteroid bullets)
    (let ((destroyed 0))

    (grab bullets) 
    (let ((closest (pdata-op "closest" "p" (get-pos asteroid))))
    (cond ((< (vdist closest (get-pos asteroid)) (asteroid-get-size asteroid))
        (set! destroyed 1)
        (set! particle-system (particle-system-add particle-system closest)))))
    (ungrab)

    (update-ob asteroid)
    
    (grab (get-obj asteroid))
    (identity)    
    (translate (get-pos asteroid))
    (concat (maim (get-dir asteroid) (vector 0 1 0)))
    (ungrab)
    destroyed))

(define (make-asteroids n l)
    (set! l (cons (make-asteroid 50 
        (vmul (rndvec) 2000)) l))
    (if (zero? n)
        l
        (make-asteroids (- n 1) l)))

(define (asteroids-update l bullets collided)
    (cond ((and (eq? collided 0) (eq? (asteroid-update (car l) bullets) 1))
        (let ((old (car l)))
        (destroy (get-obj old))
        (set! l (cdr l))
        ; uncomment for finite asteroids
        ;(cond ((> (asteroid-get-size old) 2)
            (set! l (cons (make-asteroid-child old) l))
            (set! l (cons (make-asteroid-child old) l))
            (set! l (cons (make-asteroid-child old) l));))
        (set! collided 1))))
     
    (if (null? (cdr l))
        l
        (cons (car l) (asteroids-update (cdr l) bullets collided))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-player) 
    (push)
    (hint-none)
    (hint-wire)
    (line-width 2)
    (wire-colour (vector 1 1 1))
    (scale (vector 1 1 2))
    (rotate (vector 45 0 0))
    (scale (vector 2 1 1))
    (let ((ob (build-ship)))
    (pop)    
    (apply-transform ob)
    (append (make-ob ob) (list (make-bullets)))))
    
(define (player-get-bullets player) (list-ref player 6))

(define (player-update player)
    (let ((rot (vector 0 0 0)))
    
    (if (key-pressed "q") (set! rot (vector (* (delta) -10) 0 0)))
    (if (key-pressed "a") (set! rot (vector (* (delta) 10) 0 0)))
    (if (key-pressed "o") (set! rot (vector 0 (* (delta) 10) 0)))
    (if (key-pressed "p") (set! rot (vector 0 (* (delta) -10) 0)))
    (if (key-pressed "h") 
        (begin
            (set-pos player (vmul (rndvec) WORLD_SIZE))
            (set-dir player (rndvec))))
    (if (key-pressed "z")
        (set-speed player (- (get-speed player) 1))) 
    (if (key-pressed "x")
        (set-speed player (+ (get-speed player) 1))) 
    (if (key-pressed " ")
        (begin
            (bullet-fire (player-get-bullets player) (get-pos player) 
                (vmul (get-dir player) (+ (- (get-speed player)) 500 )))))
            
    (set-rotvel player (vmul (vadd (get-rotvel player) (vmul rot 0.05)) 0.99))
    (update-ob player)
    
    (grab (get-obj player))
    (identity)    
    (translate (get-pos player))
    (concat (maim (get-dir player) (get-up player)))
    (rotate (vector 0 180 0))    
    (ungrab))
    (bullets-update (player-get-bullets player)))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-world)
    (list (make-asteroids 50 '())))

(define (world-get-asteroids world)
    (list-ref world 0))

(define (world-set-asteroids world asteroids)
    (list-set! world 0 asteroids))

(define (world-update player world)
    (world-set-asteroids world (asteroids-update (world-get-asteroids world)
        (player-get-bullets player) 0))
    (set! particle-system (particle-systems-update particle-system))
    world)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
(define (build-stars)    
    (define (init-particles n)
        (pdata-set "s" n (vector 30 30 30))
        (pdata-set "p" n (vmul (rndvec) (* WORLD_SIZE 2)))
        (pdata-set "c" n (vector 1 1 1))
        (if (zero? n)
            0
            (init-particles (- n 1))))

    (let ((p (build-particles 5000)))
    (grab p)
    (hint-none)
    (point-width 2)
    (hint-points)
    (hint-anti-alias)
    (init-particles (pdata-size))
    (ungrab)))

(define (build-ship)
    (let ((p (build-polygons 48 'triangle-list)))
    (grab p)
    ; nose
    (pdata-set "p" 0 (vector 2 0 0))
    (pdata-set "p" 1 (vector 1 0.3 -0.3))
    (pdata-set "p" 2 (vector 1 0.3 0.3))

    (pdata-set "p" 3 (vector 2 0 0))
    (pdata-set "p" 4 (vector 1 0.3 0.3))
    (pdata-set "p" 5 (vector 1 -0.3 0.3))

    (pdata-set "p" 6 (vector 2 0 0))
    (pdata-set "p" 7 (vector 1 -0.3 0.3))
    (pdata-set "p" 8 (vector 1 -0.3 -0.3))

    (pdata-set "p" 9 (vector 2 0 0))
    (pdata-set "p" 10 (vector 1 -0.3 -0.3))
    (pdata-set "p" 11 (vector 1 0.3 -0.3))

    ; back
    (pdata-set "p" 12 (vector -1 0 0))
    (pdata-set "p" 13 (vector 1 0.3 0.3))
    (pdata-set "p" 14 (vector 1 0.3 -0.3))

    (pdata-set "p" 15 (vector -1 0 0))
    (pdata-set "p" 16 (vector 1 -0.3 0.3))
    (pdata-set "p" 17 (vector 1 0.3 0.3))

    (pdata-set "p" 18 (vector -1 0 0))
    (pdata-set "p" 19 (vector 1 -0.3 -0.3))
    (pdata-set "p" 20 (vector 1 -0.3 0.3))

    (pdata-set "p" 21 (vector -1 0 0))
    (pdata-set "p" 22 (vector 1 0.3 -0.3))
    (pdata-set "p" 23 (vector 1 -0.3 -0.3))

    ; fins
    (pdata-set "p" 24 (vector 0 0 0))
    (pdata-set "p" 25 (vector -1 2 0))
    (pdata-set "p" 26 (vector -1 0 0))

    (pdata-set "p" 27 (vector 0 0 0))
    (pdata-set "p" 28 (vector -1 -2 0))
    (pdata-set "p" 29 (vector -1 0 0))

    (pdata-set "p" 30 (vector 0 0 0))
    (pdata-set "p" 31 (vector -1 0 2))
    (pdata-set "p" 32 (vector -1 0 0))

    (pdata-set "p" 33 (vector 0 0 0))
    (pdata-set "p" 34 (vector -1 0 -2))
    (pdata-set "p" 35 (vector -1 0 0))
 
    ; fins (other side)
    (pdata-set "p" 36 (vector 0 0 0))
    (pdata-set "p" 37 (vector -1 0 0))
    (pdata-set "p" 38 (vector -1 2 0))

    (pdata-set "p" 39 (vector 0 0 0))
    (pdata-set "p" 40 (vector -1 0 0))
    (pdata-set "p" 41 (vector -1 -2 0))

    (pdata-set "p" 42 (vector 0 0 0))
    (pdata-set "p" 43 (vector -1 0 0))
    (pdata-set "p" 44 (vector -1 0 2))

    (pdata-set "p" 45 (vector 0 0 0))
    (pdata-set "p" 46 (vector -1 0 0))
    (pdata-set "p" 47 (vector -1 0 -2))

    (recalc-normals 0)
    (rotate (vector 0 180 0))

    (ungrab)
    p))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(build-stars)
(define player (make-player)) ; one player
(define world (make-world))
(lock-camera (get-obj player))

(set! particle-system (particle-system-add particle-system (vector 0 0 0)))

(define (render)
    (player-update player)
    (set! world (world-update player world)))

(every-frame (render))

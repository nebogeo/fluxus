; qflux 
; GPL licence (c) dave griffiths 2006
; turn all the squares green, avoid the red dudes
; sort of influenced by q*bert

; keys:
; q : turn left
; w : turn right
; spacebar : move forward

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (rndvec)
    (vsub (vector (flxrnd) (flxrnd) (flxrnd)) (vector 0.5 0.5 0.5)))

(define (make-block height)
    (list height 0 (vector 1 1 1)))
    
(define (block-get-height block)
    (list-ref block 0))

(define (block-set-height! block height)
    (list-set! block 0 height))

(define (block-get-obj block)
    (list-ref block 1))

(define (block-set-obj! block obj)
    (list-set! block 1 obj))

(define (block-get-col block)
    (list-ref block 2))

(define (block-set-col! block col)
    (let ((obj (block-get-obj block)))
    (let ((newsquare (not (equal? (list-ref block 2) col))))
    (list-set! block 2 col)
    (cond ((not (zero? obj)) ; check we've built the object
        (grab obj)
        (colour col)
        (ungrab)))
    newsquare)))

(define (block-build! block)
    (push)
    (translate (vector 0 (block-get-height block) 0))
    (block-set-obj! block (build-cube))
    (pop))
    
(define (block-destroy block)
    (destroy (block-get-obj block)))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-world width height f)
    (define (_make n v)
        (vector-set! v n (make-block (f n)))
        (if (zero? n)
            v
            (_make (- n 1) v)))
    (list
        (vector width height)
        '()
        (_make (- (* width height) 1) (make-vector (* width height) '()))))

(define (world-get-width world)
    (vector-ref (list-ref world 0) 0))
    
(define (world-get-height world)
    (vector-ref (list-ref world 0) 1))

(define (world-get-entities world)
    (list-ref world 1))

(define (world-set-entities world entities)
    (list-set! world 1 entities))

(define (world-get-world world)
    (list-ref world 2))

(define (world-get-block world pos)
    (vector-ref (world-get-world world)
        (+ (vector-ref pos 0) (* (vector-ref pos 1)
            (world-get-width world)))))

(define (world-set-block! world pos block)
    (vector-set! (world-get-world world)
        (+ (vector-ref pos 0) (* (vector-ref pos 1)
            (world-get-width world))) block))

(define (world-build! world)
    (define (__build x y)
        (translate (vector 0 0 -1))
        (block-build! (world-get-block world (vector x y)))
        (if (zero? y)
            0
            (__build x (- y 1))))
    (define (_build x y)
        (translate (vector -1 0 0))
        (push)
        (__build x y)
        (pop)
        (if (zero? x)
            0
            (_build (- x 1) y)))
    (push)
    (line-width 4)
    (hint-wire)
    (translate (vector (world-get-width world) 0 (world-get-height world)))
    (_build (- (world-get-width world) 1) (- (world-get-height world) 1))
    (pop))
    
(define (world-destroy world)
    (define (dest n w)
        (block-destroy (vector-ref w n))
        (if (zero? n)
            0
            (dest (- n 1) w)))
    (dest (- (vector-length (world-get-world world)) 1) (world-get-world world)))
    
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-entity type x y direction)
    (list type (vector x y) direction 0 (vector 1 1 1)))
    
(define entity-size 5)

(define (entity-get-type entity)
    (list-ref entity 0))

(define (entity-get-pos entity)
    (list-ref entity 1))

(define (entity-set-pos! entity pos)
    (list-set! entity 1 pos))

(define (entity-get-dir entity)
    (list-ref entity 2))

(define (entity-set-dir! entity dir)
    (list-set! entity 2 dir))

(define (entity-get-obj entity)
    (list-ref entity 3))

(define (entity-set-obj! entity obj)
    (list-set! entity 3 obj))

(define (entity-get-col entity)
    (list-ref entity 4))

(define (entity-set-col! entity col)
    (list-set! entity 4 col))
        
(define (entity-rot-cw! entity world)
    (let ((dir (entity-get-dir entity)))
    (set! dir (+ dir 1))
    (if (> dir 3)
        (set! dir 0))
    (entity-set-dir! entity dir)
    (entity-update-obj entity world)))

(define (entity-rot-ccw! entity world)
    (let ((dir (entity-get-dir entity)))
    (set! dir (- dir 1))
    (if (< dir 0)
        (set! dir 3))
    (entity-set-dir! entity dir)
    (entity-update-obj entity world)))

(define (entity-move! entity world)
    (let ((pos (entity-get-pos entity)) (dir (entity-get-dir entity)))
    ; copy vector elements to numbers as the vector is a reference
    (let ((px (vector-ref pos 0))(py (vector-ref pos 1)))

    (cond ((eq? dir 0) (set! py (- py 1)))
    ((eq? dir 1) (set! px (+ px 1)))
    ((eq? dir 2) (set! py (+ py 1)))
    ((eq? dir 3) (set! px (- px 1))))
    
    ; wrap to the to the world limits
    (if (< px 0) (set! px 0))
    (if (< py 0) (set! py 0))
    (if (>= px (world-get-width world))  (set! px (- (world-get-width world) 1)))
    (if (>= py (world-get-height world)) (set! py (- (world-get-height world) 1)))
    
    (let ((block (world-get-block world (vector px py))))
    (cond (; if the gradient isn't too high            
            (< (abs (- (block-get-height block)
                 (block-get-height (world-get-block world pos)))) 2)
        (entity-set-pos! entity (vector px py))
        (entity-update-obj entity world)
        (if (string=? (entity-get-type entity) "p")
            (block-set-col! (world-get-block world (vector px py)) (vector 0 1 0)))))))))

(define (entity-update-obj entity world)
    (let ((obj (entity-get-obj entity)))
    (let ((pos (entity-get-pos entity)))
    (let ((height (block-get-height (world-get-block world pos))))
    (cond ((not (zero? obj)) ; check we've built the object
        (grab obj)
        (identity)
        (translate (vector (vector-ref pos 0) (+ height 1) (vector-ref pos 1)))
        (rotate (vector 0 (* (entity-get-dir entity) 90) 0))
        (scale (vector 0.5 0.5 0.5))
        (ungrab)))))))

(define (entity-build! entity)
    (push)
    (colour (entity-get-col entity))
    (specular (vector 1 1 1))
    (shinyness 20)

    ; main body
    (entity-set-obj! entity (build-sphere 15 15))
    (parent (entity-get-obj entity))

    ; nose
    (push)
    (translate (vector 0 -0.2 -1))
    (scale (vector 0.3 0.3 0.3))
    (build-sphere 5 5)
    (pop)

    ; eye 1
    (push)
    (translate (vector 0.3 0 -0.9))
    (scale (vector 0.3 0.3 0.3))
    (colour (vector 1 1 1))
    (build-sphere 3 3)

    (translate (vector 0 0 -0.5))
    (scale (vector 0.5 0.5 0.5))
    (colour (vector 0 0 0))
    (build-sphere 3 3)    
    (pop)

    ; eye 2
    (push)
    (translate (vector -0.3 0 -0.9))
    (scale (vector 0.3 0.3 0.3))
    (colour (vector 1 1 1))
    (build-sphere 3 3)

    (translate (vector 0 0 -0.5))
    (scale (vector 0.5 0.5 0.5))
    (colour (vector 0 0 0))
    (build-sphere 3 3)    
    (pop)
    (pop))

(define (entity-destroy entity)
    (destroy (entity-get-obj entity)))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-player x y direction)
    (let ((entity (append (make-entity "p" x y direction) (list 0))))
    (entity-set-col! entity (vector 0 1 0))
    entity))

(define (player-get-squarecount entity)
    (list-ref entity entity-size))

(define (player-set-squarecount! entity count)
    (list-set! entity entity-size count))

(define (player-dec-squarecount! entity)
    (list-set! entity entity-size (- (player-get-squarecount entity) 1)))

(define key-release 0)

(define (player-update player world game)

    (define (enemy-check enemies)
        (if (null? enemies) 
            0
            (cond 
                ((equal? (entity-get-pos (car enemies)) (entity-get-pos player))
                    1)
                (else 
                    (if (null? (cdr enemies))
                        0
                        (enemy-check (cdr enemies)))))))

    (cond ((eq? 1 (enemy-check (level-get-enemies (game-get-current-level game))))
        (game-set-restart! game 1)))

    (cond ((zero? (player-get-squarecount player))
        (game-set-next-level! game 1)))
    (cond
        ((key-pressed "w")
            (if (eq? key-release 0)
                (entity-rot-cw! player world))
            (set! key-release 1))
                
        ((key-pressed "q")
            (if (eq? key-release 0)
                (entity-rot-ccw! player world))
            (set! key-release 1))
                
        ((key-pressed " ")
            (if (eq? key-release 0)
                (cond ((eq? #t (entity-move! player world))
                    (player-dec-squarecount! player))))
            (set! key-release 1))
                
          (else
              (set! key-release 0))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-enemy x y direction speed program)
    (let ((entity (append (make-entity "e" x y direction) (list speed 0.0 program 0))))
    (entity-set-col! entity (vector 1 0 0))
    entity))

(define (enemy-get-tick-rate enemy)
    (list-ref enemy entity-size))

(define (enemy-set-tick-rate! enemy rate)
    (list-set! enemy entity-size rate))

(define (enemy-get-last-tick enemy)
    (list-ref enemy (+ entity-size 1)))

(define (enemy-set-last-tick! enemy last)
    (list-set! enemy (+ entity-size 1) last))

(define (enemy-get-program enemy)
    (list-ref enemy (+ entity-size 2)))

(define (enemy-get-pc enemy)
    (list-ref enemy (+ entity-size 3)))

(define (enemy-inc-pc enemy)        
    (list-set! enemy (+ entity-size 3) (modulo (+ (enemy-get-pc enemy) 1)
        (vector-length (enemy-get-program enemy)))))
        
(define (enemy-update entity world)
    (cond ((> (time) (+ (enemy-get-last-tick entity)
                        (enemy-get-tick-rate entity)))
        (enemy-inc-pc entity)
        (let ((t (vector-ref (enemy-get-program entity) (enemy-get-pc entity))))
        (cond
            ((eq? t 0) (entity-move! entity world))
            ((eq? t 1) (entity-rot-ccw! entity world))                
            ((eq? t 2) (entity-rot-cw! entity world))))            
        (enemy-set-last-tick! entity (time)))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-level size landscape camera player enemies)
    (list size landscape camera enemies player 0))

(define (level-get-size level)
    (list-ref level 0))

(define (level-get-squarecount level)
    (- (* (vector-ref (level-get-size level) 0) (vector-ref (level-get-size level) 1)) 1))

(define (level-get-landscape level)
    (list-ref level 1))

(define (level-get-camera level)
    (list-ref level 2))

(define (level-get-enemies level)
    (list-ref level 3))

(define (level-get-player level)
    (list-ref level 4))

(define (level-set-player! level player)
    (list-set! level 4 player))
    
(define (level-get-world level)
    (list-ref level 5))

(define (level-set-world! level world)
    (list-set! level 5 world))
    
(define (level-build level)
    (define (update-enemies e)
        (entity-build! (car e))
        (entity-update-obj (car e) (level-get-world level))
        (if (null? (cdr e))
            0
            (update-enemies (cdr e))))


    (let ((dim (level-get-size level)))
    (level-set-world! level (make-world (vector-ref dim 0) (vector-ref dim 1)
        (lambda (n) (vector-ref (level-get-landscape level) n))))
    (world-build! (level-get-world level))
    (entity-build! (level-get-player level))
    (entity-update-obj (level-get-player level) (level-get-world level))

    (player-set-squarecount! (level-get-player level) (level-get-squarecount level))
     
     ; set the first block to green
     (block-set-col! (world-get-block (level-get-world level)
          (entity-get-pos (level-get-player level)))
               (vector 0 1 0))
     ;(lock-camera (entity-get-obj (level-get-player level)))
     (set-camera-transform (minverse (level-get-camera level)))
     
    (if (not (eq? '() (level-get-enemies level)))
          (update-enemies (level-get-enemies level)))))

(define (level-destroy level)
    (define (destroy-enemies e)
        (entity-destroy (car e))
        (if (null? (cdr e))
            0
            (destroy-enemies (cdr e))))
    (if (not (eq? '() (level-get-enemies level)))
          (destroy-enemies (level-get-enemies level)))
    (world-destroy (level-get-world level))
    (entity-destroy (level-get-player level)))

(define (level-update level game)
    (define (update-enemies e)
        (enemy-update (car e) (level-get-world level))
        (if (null? (cdr e))
            0
            (update-enemies (cdr e))))
        
    (if (not (eq? '() (level-get-enemies level)))
          (update-enemies (level-get-enemies level)))
    (player-update (level-get-player level) (level-get-world level) game))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-game levels)
    (list 0 levels 0 0))

(define (game-get-level-no game)
    (list-ref game 0))

(define (game-set-level-no! game n)
    (list-set! game 0 n))

(define (game-inc-level-no! game)
    (game-set-level-no! game (modulo (+ (game-get-level-no game) 1) 
        (length (game-get-levels game)))))

(define (game-get-levels game)
    (list-ref game 1))

(define (game-get-current-level game)
    (list-ref (game-get-levels game) (game-get-level-no game)))

(define (game-get-next-level game)
    (list-ref game 2))

(define (game-set-next-level! game reset)
    (list-set! game 2 reset))

(define (game-set-restart! game reset)
    (list-set! game 3 reset))

(define (game-get-restart game)
    (list-ref game 3))
    
(define (game-build game)
    (level-build (game-get-current-level game)))

(define (game-update game)
    (cond ((eq? (game-get-next-level game) 1)
        (game-set-next-level! game 0)
        (level-destroy (game-get-current-level game))
        (game-inc-level-no! game)
        (level-build (game-get-current-level game))))

    (cond ((eq? (game-get-restart game) 1)
        (game-set-restart! game 0)
        (level-destroy (game-get-current-level game))
        (level-build (game-get-current-level game))))
        
    (level-update (game-get-current-level game) game))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(clear)
(clear-colour (vector 0 0.4 0.8))
(ortho)

(define game (make-game (list

     (make-level
            (vector 3 3)
            (vector 1 1 1
                    1 1 1
                    1 1 1 )
            (mmul
                    (mrotate (vector 45 160 20))
                    (mtranslate (vector 0 1 15 )))
            (make-player 1 1 0)
            (list)) 
            
     (make-level
            (vector 5 5)
            (vector 1 1 1 1 1
                    4 5 5 5 2
                    4 8 9 6 2
                    4 7 7 6 2
                    3 3 3 3 2 )
            (mmul
                    (mrotate (vector -42 35 -27))
                    (mtranslate (vector 3 3 15)))
            (make-player 0 0 2)
            (list))

      (make-level
            (vector 5 5)
            (vector 1 1 1 1 1
                    1 2 2 2 1
                    1 2 3 2 1
                    1 2 2 2 1
                    1 1 1 1 1 )
            (mmul
                    (mrotate (vector -45 45 -34))
                    (mtranslate (vector 3 1 10)))
            (make-player 2 2 3)
            (list
                (make-enemy 0 4 0 0.5 (vector 0 0 0 0 1))))        

 
    (make-level
            (vector 7 7)
            (vector 3 3 3 2 3 3 3
                    3 3 3 1 3 3 3
                    3 3 3 1 3 3 3
                    2 1 1 1 1 1 2
                    3 3 3 1 3 3 3
                    3 3 3 1 3 3 3
                    3 3 3 2 3 3 3 )
            (mmul
                    (mrotate (vector -80 25 -35))
                    (mtranslate (vector 5 0 10)))
            (make-player 1 1 0)
            (list
                (make-enemy 0 0 0 0.5 (vector 0 0 1))
                (make-enemy 0 6 0 0.75 (vector 0 1 0))
                (make-enemy 6 0 0 0.6 (vector 1 0 0))
                (make-enemy 6 6 0 0.4 (vector 0 0 1))))        
                
  (make-level
            (vector 10 10)
            (vector 5 6 6 7 7 8 8 9 9 10
                    5 5 6 6 7 7 8 8 9 9
                    4 5 5 6 6 7 7 8 8 9
                    4 4 5 5 6 6 7 7 8 8
                    3 4 4 5 5 6 6 7 7 8
                    3 3 4 4 5 5 6 6 7 7
                    2 3 3 4 4 5 5 6 6 7
                    2 2 3 3 4 4 5 5 6 6
                    1 2 2 3 3 4 4 5 5 6
                    1 1 2 2 3 3 4 4 5 5)
            (mmul
                    (mrotate (vector -45 65 -40))
                    (mtranslate (vector 5 6 100)))
            (make-player 0 0 0)
            (list
                (make-enemy 4 4 2 0.6 (vector 1 0))
                (make-enemy 3 5 2 0.5 (vector 0 0 1 0))
                (make-enemy 2 4 2 0.4 (vector 0 0 0 0 1 0))
                (make-enemy 1 3 2 0.3 (vector 0 0 0 0 0 0 1 0))
                (make-enemy 0 2 2 0.2 (vector 0 0 0 0 0 0 0 0 1 0))))
                
  (make-level
            (vector 10 10)
            (vector 9 9 9 9 9 9 9 9 9 9
                    9 9 9 1 1 1 1 9 9 9
                    9 9 9 1 1 1 1 9 9 9
                    8 1 1 1 4 4 1 1 1 9 
                    7 2 1 4 4 4 4 1 1 9
                    6 3 1 4 4 4 4 1 1 9
                    5 4 4 4 4 4 1 1 1 9
                    9 9 9 1 1 1 1 9 9 9
                    9 9 9 1 1 1 1 9 9 9
                    9 9 9 9 9 9 9 9 9 9)
            (mmul 
                    (mrotate (vector 0 90 0))
                (mmul
                    (mrotate (vector -25 45 -20))
                    (mtranslate (vector 0 6 100)))
                    )
            (make-player 4 5 0)
            (list
                (make-enemy 0 0 1 0.1 (vector 
        0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 2 2
        0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 2 2))
                (make-enemy 7 1 1 0.5 (vector 2 0 2 0))
                (make-enemy 1 7 1 0.5 (vector 2 0 2 0))
                (make-enemy 7 7 1 0.5 (vector 0 2 0 2))
                (make-enemy 3 1 1 0.3 (vector 0 0 0 2 0 2))
                (make-enemy 4 5 1 0.5 (vector 0 0 2))
            ))
               
   
            
    )))

(game-build game)

(define (render)
    (game-update game))

(every-frame (render))

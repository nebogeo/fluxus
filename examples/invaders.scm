;; Fluxus Invaders
;; Copyright (C) 2009 Gabor Papp
;; GNU GPL v3

;; hide the code with ctrl-h
;; keys: cursor keys + space

(require scheme/class)
(require scheme/list)
(require scheme/math)

(clear)

(hint-ignore-depth)
(hint-unlit)

(texture-params 0
    '(min nearest
        mag nearest
        wrap-s clamp
        wrap-t clamp))

(define texture-location "invaders/")

;; load invader textures
(define invader-textures
    (build-list 3
        (lambda (i)
              (for/list ([j (in-range 0 2)])
                (load-texture (string-append texture-location
                                            "invader"
                                            (number->string i)
                                            (number->string j)
                                            ".png"))))))
(define invader-texture-killed
  	(load-texture (string-append texture-location "invaderxx.png")))

;; missile textures
(define missile-textures
  (build-list 3
        (lambda (i)
          (if (= i 2)
            (list (load-texture (string-append texture-location
                                               "missile20.png")))
            (for/list ([j (in-range 0 2)])
                (load-texture (string-append texture-location
                                             "missile"
                                             (number->string i)
                                             (number->string j)
                                             ".png")))))))

;; ship textures
(define ship-textures
  (for/list ([j (in-range 0 3)])
        (load-texture (string-append texture-location
                                    "ship0"
                                    (number->string j)
                                    ".png"))))
; area borders
(define border-left -120)
(define border-right 120)
(define border-bottom -190)
(define border-top 50)

(define (draw-rect v)
  (let ([x0 (vector-ref v 0)]
        [y0 (vector-ref v 1)]
        [x1 (vector-ref v 2)]
        [y1 (vector-ref v 3)]
        [rect (build-polygons 4 'polygon)])
    (with-primitive rect
        (pdata-set! "p" 0 (vector x0 y0 0))
        (pdata-set! "p" 1 (vector x1 y0 0))
        (pdata-set! "p" 2 (vector x1 y1 0))
        (pdata-set! "p" 3 (vector x0 y1 0))
        (hint-none)
        (hint-wire)
        (line-width 1)
        (wire-colour (vector 1 0 0)))
    rect))

(define (collide? a b)
  (let-values ([(ax0 ay0 ax1 ay1) (vector->values a)]
               [(bx0 by0 bx1 by1) (vector->values b)])
        (cond [(and (< ax0 bx0 ax1) (< ay0 by0 ay1)) (vector bx0 by0)]
              [(and (< ax0 bx0 ax1) (< ay0 by1 ay1)) (vector bx0 by1)]
              [(and (< ax0 bx1 ax1) (< ay0 by1 ay1)) (vector bx1 by1)]
              [(and (< ax0 bx1 ax1) (< ay0 by0 ay1)) (vector bx1 by0)]
			  [(and (< bx0 ax0 bx1) (< by0 ay0 by1)) (vector ax0 ay0)]
              [(and (< bx0 ax0 bx1) (< by0 ay1 by1)) (vector ax0 ay1)]
              [(and (< bx0 ax1 bx1) (< by0 ay1 by1)) (vector ax1 ay1)]
              [(and (< bx0 ax1 bx1) (< by0 ay0 by1)) (vector ax1 ay0)]
              (else #f))))

(define ship%
  (class object%
		(define start-x -90)

        (define x start-x)
        (define y -160)

		(define death-time 0)	; time of death

        (define frames
                (build-list 3
                    (lambda (x)
                        (with-state
                          (texture (list-ref ship-textures x))
                          (build-plane)))))

         ;(define bb 0)
         (define missile #f)    ; missile object or #f

		 (define state 'alive)	; 'alive or 'dying

         (define/public (update barricades)
			(cond [(eq? state 'alive)
							(when (and (key-special-pressed 100)    ; left cursor
									   (>= x border-left))
							  (set! x (- x 2)))
							(when (and (key-special-pressed 102)    ; right cursor
									   (<= x border-right))
							  (set! x (+ x 2)))
							(when (key-pressed " ")
							  (shoot))

							; renders the sprite
							(with-primitive (car frames)
									(hide 0)
									(identity)
									(translate (vector x y 0))
									(scale (vector 16 8 1)))
							(for-each (lambda (f)
										(with-primitive f
												(hide 1))) (cdr frames))]
				  [(eq? state 'dying)
							(for-each (lambda (f)
										(with-primitive f
												(hide 1))) frames)
							(let [(anim-frame (inexact->exact (modulo (floor (* 5 (flxtime))) 2)))]
								(with-primitive (list-ref frames (add1 anim-frame))
										(hide 0)
										(identity)
										(translate (vector x y 0))
										(scale (vector 16 8 1))))
							(when (> (- (flxtime) death-time) 3)
							  (set! state 'alive)
							  (set! x start-x))])

            ; updates the missile if there's any
            (when missile
              (send missile update))

            (when (or (and missile (send missile outside?)) ; deletes the missile if it leaves the screen
					  ; check missile-barricades collision
					  (and missile (ormap (lambda (barricade) 
											(send barricade collide (send missile bounding-box)))
										 barricades)))
              (kill-missile))
            
            ;(destroy bb)
            ;(set! bb (draw-rect (bounding-box)))
			)

         ; makes a new missile if there is no current one
         (define (shoot)
           (unless missile
              (set! missile (make-object missile% x y 2 2))))

         (define/public (bounding-box)
				(if (eq? state 'alive)
                    (vector (- x 7) (- y 4) (+ x 8) (+ y 4))
					(vector -1000 -1000 -1000 -1000)))

		 ; returns the bounding box of missile
		 (define/public (missile-bounding-box)
			(if missile
			  (send missile bounding-box)
			  ; if there's no missile return a hypothetical box that does not
			  ; collides with the invaders
			  (vector -1000 -1000 -1000 -1000)))

		 ;; missile is destroyed because it shot an invader, collided into a
		 ;; barricade or out of the screen
		 (define/public (kill-missile)
			(send missile delete)
			(set! missile #f))

		 (define/public (kill)
			(set! death-time (flxtime))
			(set! state 'dying))

         (super-new)))

;; barricade
(define barricade%
  (class object%
         (init x_ y_)

         (define x x_) ; bottom-left corner of the barricade
         (define y y_)

         (define pix (load-primitive (string-append texture-location
                                                    "barricade.png")))

         ; texture is 24x18 stored in the top-left corner of a 32x32 image
         (define width (with-primitive pix (pixels-width)))
         (define height (with-primitive pix (pixels-height)))

         (define bw 6) ; block width
         (define bh 6) ; block height
		 
         (define tex (pixels->texture pix))

         ; move out of the screen, so it cannot be seen
         (with-primitive pix
                         (translate (vector 10000 0 0)))

         (define barricade (build-plane))
         (with-primitive barricade
                         (texture tex)
                         (translate (vector x y 0))
                         (scale (vector width height 1))
                         (translate (vector .5 -.5 0)))

         ; the barricade is subdivided to 4x3 parts, pixel indices are shuffled
         ; and pixels are deleted from the texture if the part is hit by a missile
         (define pixel-list
           (list->vector
               (for*/list ([yc (in-range 0 3)]    ; blocks
                           [xc (in-range 0 4)])
                          (if (and (or (= xc 1) (= xc 2)) (= yc 2))
                              '() ; empty area in barricade
                              (let ([i (+ (* xc bw) (* yc bh width))])
                                (sort 
                                    (for*/list ([x (in-range 0 bw)]    ; list of pixel offsets in block
                                                [y (in-range 0 bh)])
                                                   (+ (* width y) x i))
                                    < #:key (lambda (x) (random)) #:cache-keys? #t)))))) ; shuffle the list

         ; erodes block i in barricade
         ; returns #t on collision
         (define (erode i)
            (if (null? (vector-ref pixel-list i))
                  #f
                (begin
                    (with-primitive pix
                        (for ([j (in-range 0 9)])
							(let* ([o (list-ref (vector-ref pixel-list i) j)] ; mirror y
								   [ox (modulo o width)]
								   [oy (- (- height 1) (quotient o width))])
								(pdata-set! "c" (+ ox (* oy width)) (vector 0 0))))
                        (pixels-upload))
                    (vector-set! pixel-list i (list-tail (vector-ref pixel-list i) 9))
                    #t)))

         ; check collision with m-bb bounding box
         ; returns #t on collision
         (define/public (collide m-bb)
            (let ([collision-point (collide? (bounding-box) m-bb)])
              (if collision-point
                (let* ([bx (quotient (- (vx collision-point) x) bw)]
                       [by (quotient (- y (vy collision-point)) bh)]
                       [bi (+ bx (* by 4))])
                    (erode bi))
                #f)))

         (define/public (bounding-box)
                    (vector x (- y 18) (+ x 24) y))

         ;(draw-rect (bounding-box))

         (super-new)))

(define missile%
      (class object%
        (init x_ y_ yvel_ type_)
        
        (define x x_)
        (define y y_)
        (define yvel yvel_)

        (define frames
          (let ([txts (list-ref missile-textures type_)])
                (build-list (length txts)
                    (lambda (x)
                        (with-state
                          (texture (list-ref txts x))
                          (build-plane))))))

        (define anim-frame 0) ; current animation frame index
        (define anim-fraction-counter 0)

        ;(define bb 0) ; bounding box object

        (define/public (update)
            (let ([cframe (list-ref frames anim-frame)])
                (for-each
                  (lambda (f)
                    (with-primitive f
                            (hide 1)))
                  frames)
                (with-primitive cframe
                    (hide 0)
                    (identity)
                    (translate (vector x y 0))                       
                    (scale (vector 4 8 1)))
                (set! anim-fraction-counter (add1 anim-fraction-counter))
                (when (zero? (bitwise-and anim-fraction-counter 3))
                    (set! anim-frame (modulo (add1 anim-frame) (length frames)))))
            ;(destroy bb)
            ;(set! bb (draw-rect (bounding-box)))
            (set! y (+ y yvel)))

        (define/public (bounding-box)
                (vector (- x 2) (- y 4) (+ x 1) (+ y 1)))

        ;; if its outside the play area
        (define/public (outside?)
            (not (< border-bottom y border-top)))

        (define/public (get-x)
            x)

        (define/public (get-y)
            y)

        ;; delete resources
        (define/public (delete)
			(set! y (- border-bottom 1))
            ;(destroy bb)
            (for-each destroy frames))

        (super-new)))

(define invader%
    (class object%
        (init type_ x_ y_)
        
        (when (>= type_ (length invader-textures))
            (error "invader: invalid type"))
        
        (define type type_) ; invader type      
        (define x x_) ; position
        (define y y_)

        (define agressive #f)
        
        ; animation frames
        (define frames 
            (let ([txts (list-ref invader-textures type)])
                (build-list 2
                    (lambda (x)
                        (with-state
                            (texture (list-ref txts x))
                            (build-plane))))))

        (define anim-frame 0) ; current animation frame index
		(define state 'alive) ; 'alive, 'dying or 'dead
        
        ;(define bb 0)

        (define/public (update speed)
			(when (and (eq? state 'dying)
					   (not (zero? speed)))
			  (set! state 'dead))
			(cond [(eq? state 'alive)
				   			(set! x (+ x speed))
							(let ([i0 anim-frame]
								  [i1 (bitwise-xor anim-frame 1)])
								(with-primitive (list-ref frames i0)
									(hide 0)
									(identity)
									(translate (vector x y 0))                            
									(scale (vector 16 8 1)))
								(with-primitive (list-ref frames i1)
									(hide 1))
								(set! anim-frame i1))
							;(destroy bb)
							;(set! bb (draw-rect (bounding-box)))
							]
				  [(eq? state 'dying)
				   			;(destroy bb)
				   			(with-primitive (list-ref frames 0)
									(hide 0)
									(texture invader-texture-killed)
									(identity)
									(translate (vector x y 0))
									(scale (vector 16 16 1)))
							(with-primitive (list-ref frames 1)
									(hide 1))]
				  [(eq? state 'dead)
				   			(with-primitive (list-ref frames 0)
									(hide 1))
							(with-primitive (list-ref frames 1)
									(hide 1))]
				  ))

        (define/public (bounding-box)
            (case type
              ((list 0) (vector (- x 6) (- y 4) (+ x 6) (+ y 4)))
              ((list 1) (vector (- x 5) (- y 4) (+ x 6) (+ y 4)))
              ((list 2) (vector (- x 4) (- y 4) (+ x 4) (+ y 4)))))

        (define/public (get-x)
            x)
 
        (define/public (get-y)
            y)

		(define/public (step-down)
			(set! y (- y 2)))

        (define/public (set-agression a)
            (set! agressive a))

        (define/public (is-shooting?)
            (and agressive (zero? (random 20))))

        (define/public (get-missile-position)
            (values (- x 1) (- y 5)))

		;; invader is shot down
		(define/public (kill)
			(set! state 'dying)
			(update 0))

		(define/public (dead?)
			(eq? state 'dead))

		(define/public (delete)
			;(destroy bb)
			(destroy (list-ref frames 0))
			(destroy (list-ref frames 1)))

        (super-new)))

(define invaders%
    (class object%
        (define invaders
            (for*/list ([x (in-range -5 6)]
                    [y (in-range 0 5)])
                (make-object invader% (- 2 (truncate (/ (+ y 1) 2)))
                    (* x 16) (* (- y) 16))))

		(define invaders-max (length invaders))

        (define step 2) ; invader step size including direction 2 or -2

		(define start-time (flxtime)) ; start of game

		(define update-time-max 1) ; maximum update time, slowest speed
		(define update-time-min .01) ; minimum update time, fastest speed
        (define update-time update-time-max) ; time between updates in seconds

        (define last-frame-update 0)  ; last animation update
        
        (define missiles '()) ; list of active missiles

        (define barricades
          (for/list ([x (in-range -90 91 60)])
              (make-object barricade% (- x 12) -130)))

        (define player
          (make-object ship%))

		(define (set-update-time)
		  	(let  ([d (/ (- invaders-max (length invaders)) invaders-max)])
			  	(set! update-time
				  			(if (> d 1.0)
							  update-time-min
							  (+ update-time-min (* (- update-time-max update-time-min) (sqr (- 1 d))))))))
			
        ;; updates invaders behaviour
        (define/public (update)
            (let ([current-time (flxtime)])
			  	(set-update-time)
                ; less frequent updates
                (when (>= (- current-time last-frame-update) update-time)
                    (for-each
                        (lambda (invader)
                            (send invader update step))
                        invaders)
                    ; change direction if border is reached
                    (when (or (<= (get-border-x step) border-left)
                              (>= (get-border-x step) border-right))
                      (set! step (* -1 step))
					  ; make invaders step down
					  (for-each
						(lambda (invader)
						  (send invader step-down))
						invaders))

                    (set-invaders-agression)
                    (new-missiles)
                    (set! last-frame-update current-time))
                ; on every call
				(check-ship)
                (update-missiles)
				(check-invaders) ; invaders-missile collision 
                (send player update barricades)))

        ;; create new missiles
        (define (new-missiles)
          (for-each (lambda (invader)
                      (let ([ms (send invader is-shooting?)]
                            [x 0]
                            [y 0])
                            (when ms
                              (set!-values (x y) (send invader get-missile-position))
                              (set! missiles (cons (make-object missile% x y -2 (random 2)) missiles)))))
                    invaders))

        ;; update missiles
        (define (update-missiles)
          (for-each (lambda (missile)
                      (send missile update))
                    missiles)
          ; collisions
          (remove-missiles))

        ;; remove missiles
        (define (remove-missiles)
          (let* ([outside-missiles (filter (lambda (m) (send m outside?)) missiles)]
                 [collided-missiles (filter (lambda (missile)
                                              (ormap (lambda (barricade)
                                                (send barricade collide (send missile bounding-box)))
                                                barricades))
                                      missiles)]
                [dead-missiles (append outside-missiles collided-missiles)])

            (for-each (lambda (m) (send m delete)) dead-missiles)
            (set! missiles (filter-not (lambda (m) (member m dead-missiles)) missiles))))

        ;; returns the x coordinate of the leftmost or rightmost invader
        ;; dir - number, sign specifies the left or right side
        ;;          negative - left side, positive - right side
        (define (get-border-x dir)
          (let* ([op (if (negative? dir) < >)]
                 [start-value (if (negative? dir) 9999 -9999)])
                (foldl
                  (lambda (invader border-x)
                    (let ([x (send invader get-x)])
                      (if (op x border-x)
                        x
                        border-x)))
                  start-value
                  invaders)))

		;; check for killed invaders
		(define (check-invaders)
		  (for-each (lambda (invader)
					  (when (collide?
							  (send invader bounding-box)
							  (send player missile-bounding-box))
					  	(send invader kill)
						(send player kill-missile)))
					invaders)

		  ; remove dead invaders
          (let ([dead-invaders (filter (lambda (i) (send i dead?)) invaders)])
				(for-each (lambda (i) (send i delete)) dead-invaders)
				(set! invaders (filter-not (lambda (i) (member i dead-invaders)) invaders))))

		(define (check-ship)
		  (for-each (lambda (missile)
					  (when (collide?
							  (send player bounding-box)
							  (send missile bounding-box))
						(send missile delete)
						(send player kill)
						(set! missiles (filter-not (lambda (m) (eq? m missile)) missiles))))
					  missiles))

        ;; invaders at the bottom are shooting only
        (define (set-invaders-agression)
          (let* ([y-invaders (sort invaders (lambda (i j)
                                              (> (send i get-y) (send j get-y))))] ; invaders sorted by y-coord
                 [x-hash (for/hash ([inv y-invaders])
                          (values (send inv get-x) inv))]) ; highest y per x
                ; clear agression flag of each invader
                (for-each (lambda (inv) (send inv set-agression #f)) invaders)
                ; set agression flag of invaders at the bottom
                (hash-map x-hash (lambda (key inv) (send inv set-agression #t)))))

        (super-new)))

(set-camera-transform 
        (mtranslate (vector 0 80 -160)))

(define invaders (make-object invaders%))

(define (mainloop)
    (send invaders update))

(every-frame (mainloop))


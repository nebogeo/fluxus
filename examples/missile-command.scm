; missile command - requires glsl support
; click mouse to lauch missiles

(require fluxus-018/fluxa)

; use this to create livecoded abstract machinima,
; by hacking it in live performances...

(eq 0 1 0.4)

; inline glsl:

(define nuke-vert "
    uniform vec3 LightPos;
    varying vec3 N;
    varying vec3 P;
    varying vec3 V;
    varying vec3 L;
    varying vec4 S;
    
    void main()
    {    
        N = normalize(gl_NormalMatrix*gl_Normal);
        P = gl_Vertex.xyz;
        V = -vec3(gl_ModelViewMatrix*gl_Vertex);
        L = vec3(gl_ModelViewMatrix*(vec4(LightPos,1)-gl_Vertex));
        gl_Position = ftransform();    
        S = gl_ProjectionMatrix*gl_Position;
        
        }
    ")

(define nuke-frag "
    
    uniform vec4 Colour1;
    uniform vec4 Colour2;
    uniform vec4 Colour3;
    uniform vec4 Colour4;
    uniform float Blob1;
    uniform float Blob2;
    uniform float Blob3;
    uniform float Time;
    uniform float foo;
    
    varying vec3 P;
    varying vec3 N;
    varying vec3 L;
    varying vec3 V;
    varying vec4 S;
    
    void main()
    { 
        vec3 n = normalize(N);
        vec3 l = normalize(L);
        vec3 v = normalize(V);
        vec3 s = vec3(S);
        float t = Time;
        float lambert = dot(l,n);
        vec4 colour = Colour1;
        
        colour = Colour1;
        
        if (dot(n,v)<0.95+(sin(n.y*Blob1+t)+cos(n.z*Blob1+t))*0.1) colour = Colour2;
        if (dot(n,v)<0.7+(sin(n.y*Blob2+t)+cos(n.z*Blob2+t))*0.1) colour = Colour3;
        if (dot(n,v)<0.5+(sin(n.y*Blob3+t)+cos(n.z+Blob3+t))*0.1) colour = Colour4;
        
        gl_FragColor = colour;
        }
    
    ")

;--------------------------------------------

(define-struct city
    (root pos (age #:mutable)))

(define (add-city cities pos)
    (let ((root (with-state
                    (with-state
                        (translate pos)
                        (colour (vmul (vector 0.5 0.5 0.5) (abs (grndf))))
                        (rotate (vector 0 (random 360) 0))
                        (translate (vmul (vector (abs (grndf)) 0 (abs (grndf)))
                                0.4))
                        (scale (vmul (vector (abs (grndf)) 
                                    (* 3 (abs (grndf))) 
                                    (abs (grndf)))
                                
                                0.5))
                        
                        (hint-cast-shadow)            
                        (let ((r (build-cube)))
                            (with-state 
                                (parent r)
                                (colour (vector 1 1 0.5))
                                (emissive (vector 1 1 0.5))
                                (hint-unlit)
                                ;     (hint-ignore-depth)
                                (hint-depth-sort)
                                (translate (vmul (crndvec) 0.01))
                                (scale 1.01)
                                (texture (load-texture "lights.png"))
                                (build-cube))
                            r)))))
        (with-primitive root 
            (apply-transform) 
            (scale (vector 1 0 1)))        
        
        (cons (make-city root pos 0) cities)))

(define (update-cities cities explosions)
    (filter
        (lambda (city)                          
            (set-city-age! city (+ (city-age city) (delta)))
            (when (< (city-age city) 3)
                (with-primitive (city-root city)
                    (identity)
                    (scale (vector 1 (/ (city-age city) 3) 1)))) 
            (foldl
                (lambda (explosion ret)
                    (cond ((and ret (> (explosion-radius explosion)
                                    (vdist (explosion-pos explosion)
                                        (city-pos city))))
                            (play-now (mul (mul (adsr 0.2 0.3 0 0) 10)
                                    (mooglp (pink (add 3 (mul 1000 (sine 50)))) 0.1 0.5)))
                            (destroy (city-root city))
                            #f)
                        (else ret)))                               
                #t explosions))
        cities))

;--------------------------------------------

(define-struct bomb 
    (root (pos #:mutable) vel split))

(define (add-bomb bombs pos)
    (cons (make-bomb 
            (with-state 
                (emissive (vector 0.5 0 0))
                (colour (vector 1 0 0))
                (translate pos) 
                (scale (vector 0.1 0.1 0.1))                              
                (build-sphere 5 5)) pos 
            (vadd (vector 0 -0.01 0) 
                (vmul (vector (grndf) (- (abs (grndf))) 0) 0.5))
            (* (rndf) 10)) bombs))

(define (update-bombs bombs explosions)
    (filter
        (lambda (bomb)                
            (with-primitive (bomb-root bomb)
                (translate (bomb-vel bomb))
                (set-bomb-pos! bomb (vtransform (vector 0 0 0) (get-transform))))
            
            (cond ((< (vy (bomb-pos bomb)) 0)
                    (add-bomb-explosion (bomb-pos bomb))
                    (destroy (bomb-root bomb))
                    #f)
                (else 
                    (foldl
                        (lambda (explosion ret)
                            (cond ((and ret (> (explosion-radius explosion)
                                            (vdist (explosion-pos explosion)
                                                (bomb-pos bomb))))
                                    (play-now (mul (adsr 0.2 0.3 0 0)
                                            (saw (add 880 (mul 1000 (sine 50))))))
                                    (destroy (bomb-root bomb))
                                    #f)
                                (else ret)))                               
                        #t explosions))))
        bombs))

;--------------------------------------------

(define-struct explosion 
    (root pos (radius #:mutable) colour))

(define (update-explosions explosions)
    (filter 
        (lambda (explosion)
            (with-primitive (explosion-root explosion)
                (shader-set! (list "Time" (* (time) 10)))
                (identity) ; want to scale linearly, so have to reset trasform
                (translate (explosion-pos explosion))            
                (light-diffuse l (vmul (explosion-colour explosion) 
                        (/ (explosion-radius explosion) 3)))
                (scale (explosion-radius explosion)))
            (set-explosion-radius! explosion (+ (explosion-radius explosion) (delta)))
            (cond ((> (explosion-radius explosion) 3)
                    (destroy (explosion-root explosion)) ; kill the explosion
                    (reset-light)
                    #f)
                (else #t)))                    
        explosions))

;--------------------------------------------

(define-struct player 
    (pos missiles  (next-missile #:mutable) num-missiles))

(define (build-player pos num-missiles)
    (let ((p (build-particles num-missiles)))
        (with-primitive p
            (pdata-add "vel" "v")
            (pdata-add "dest" "v")
            (pdata-map!
                (lambda (p)
                    pos)
                "p")
            (pdata-map!
                (lambda (c)
                    (vector 0 0 1))
                "c")
            (pdata-map!
                (lambda (dest)
                    (vector 1 1000 1)) ; make the destination far away so we 
                "dest"))               ; don't explode in the silo
        (make-player pos p 0 num-missiles)))

(define (get-pos-from-mouse)
    (let* ((ndcpos (vector (* (- (/ (mouse-x) (vx (get-screen-size))) 0.5) 20)
                    (* (- (- (/ (mouse-y) (vy (get-screen-size))) 0.5)) 14) -10))
            (scrpos (vtransform ndcpos (minverse (get-camera-transform)))))
        scrpos))

(define (get-vel-from-mouse player)
    (vmul (vnormalise (vsub (get-pos-from-mouse) (player-pos player))) 0.3))

(define (launch-missile player)
    (play-now (mul (white (mul 10 (adsr 0 0.5 0 0))) (adsr 0 0.3 0 0)))
    (with-primitive (player-missiles player)
        (pdata-set! "vel" (player-next-missile player) (get-vel-from-mouse player))
        (pdata-set! "p" (player-next-missile player) (player-pos player))
        (pdata-set! "dest" (player-next-missile player) (get-pos-from-mouse)))
    (set-player-next-missile! player
        (modulo (+ (player-next-missile player) 1) 
            (player-num-missiles player))))


(define (update-player player)
    (when (mouse-button 1)
        (launch-missile player))
    
    (with-primitive (player-missiles player)
        (pdata-op "+" "p" "vel")
        (pdata-index-map! 
            (lambda (i p dest)
                (cond ((< (vdist p dest) 0.4) ; time to explode!
                        (pdata-set! "vel" i (vector 0 0 0))
                        (add-player-explosion p)
                        (pdata-set! "dest" i (vector 0 1000 0)) ; stop exploding
                        (player-pos player)) ; reset position
                    (else p)))
            "p" "dest")))

;---------------------------------------

(define player-explosions '())
(define bomb-explosions '())
(define bombs '())
(define cities '())

(define (add-player-explosion pos)
    (play-now (mul (mul 5 (mooglp (add (saw 34) (saw 34.4)) (mul 0.1 (adsr 0 0.5 0.1 5)) 0.1)) 
            (adsr 0 0.7 0.5 3)))
    
    (light-position l pos)
    (light-diffuse l (vector 0 0 1))
    (let ((obj (with-state
                    (shader-source nuke-vert nuke-frag)
                    (colour (vector 0 0 1))
                    (translate pos)
                    (scale 0.01)
                    (build-sphere 10 10))))
        
        (with-primitive obj                
            (shader-set! 
                (list 
                    "Blob1" 1.2
                    "Blob2" 3.0
                    "Blob3" 4.0
                    "Colour1" (vector 1 1 1 1)
                    "Colour2" (vector 1 0 1 1)
                    "Colour3" (vector 0 0 1 1)
                    "Colour4" (vector 0 0 0 1)
                    )))
        
        (set! player-explosions (cons (make-explosion obj pos 0
                    (vector 1 0 1)) player-explosions))))

(define (add-bomb-explosion pos)
    (play-now (mul (mul 5 (mooglp (white 1) (mul 0.1 (adsr 0 0.5 0.1 5)) 0.1)) 
            (adsr 0 0.7 0.5 3)))
    (light-position l (vadd pos (vector 0 1 0)))
    
    (let ((obj (with-state
                    (shader-source nuke-vert nuke-frag)
                    (colour (vector 1 0 0))
                    (translate pos)
                    (scale 0.01)
                    (build-sphere 10 10))))
        
        (with-primitive obj                
            (shader-set! 
                (list 
                    "Blob1" 10.2
                    "Blob2" 30.0
                    "Blob3" 40.0
                    "Colour1" (vector 1 1 1 1)
                    "Colour2" (vector 1 1 0 1)
                    "Colour3" (vector 1 0 0 1)
                    "Colour4" (vector 0 0 0 1)
                    )))
        
        (set! bomb-explosions (cons (make-explosion obj pos 0 (vector 1 1 0)) 
                bomb-explosions))))


(define (reset-light)
    (light-diffuse l (vector 0 0 0)))

; main ---------------------------------

(clear)
(clear-colour (vector 0.1 0.1 0.2))
(blur 0)

(define l (make-light 'spot 'free))
(reset-light)
(light-diffuse 0 (vector 0 0 0))
(shadow-light 0)

(define bg (make-light 'spot 'free))
(light-position bg (vector 100 100 -10))
(light-diffuse bg (vector 0.1 0.1 0.1))

(set-camera-transform (mtranslate (vector 0 -6 -10)))

(with-state
    (colour (vector 0.1 0.1 0.1))
    (scale (vector 50 1 15))
    (rotate (vector -90 0 0))
    (build-seg-plane 10 10))

(for ((i (in-range 0 10)))
    (with-state
        (colour 0.2)
        (translate (vector (* 25 (crndf)) 0 -10))
        (rotate (vector 45 0 (+ (* (grndf) 10) 45)))
        (scale (vmul (rndvec) 10))
        (build-cube)))

(define myplayer (build-player (vector 0 0 0) 100))

(for ((i (in-range 0 100)))
    (set! cities (add-city cities (vector (* (crndf) 10) 0 (* (crndf) 2)))))

(define time-now 0)
(define tick 0.5)
(define time-next 0)

(define (animate)
    (when (zero? (random 100))
        (set! bombs (add-bomb bombs (vector (* (crndf) 5) 13 0)))
        (play-now (mul (adsr 0.5 0 0 0) (saw (add 100 (mul 100 (adsr 0 0.5 0 0)))))))
    (update-player myplayer)
    (set! player-explosions (update-explosions player-explosions))
    (set! bomb-explosions (update-explosions bomb-explosions))
    (set! bombs (update-bombs bombs player-explosions))
    (set! cities (update-cities cities bomb-explosions))
    
    (set! time-now (+ time-now (delta)))
    (when (> time-now time-next)
        (set! time-next (+ time-next tick))
        (when (< (length cities) 100) 
            (play-now (mul (mul 0.05 (adsr 0 0 1 1)) 
                    (saw (add 500 (mul (mul 10000 (adsr 0 0.5 0 0)) (white 0.6))))))
            (set! cities (add-city cities (vector (* (crndf) 10) 0 (* (crndf) 2)))))) #;(update-osc-rec))

(every-frame (animate))

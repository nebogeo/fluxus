; a simple use of physics without collisions
; you need audio to see this work, it's a favorite of mine :)
(start-audio "alsa_pcm:capture_1" 1024 44100)

; global control constants
(define speed 10)
(define rotsp 2)

(collisions 0)

; create a cube of dimensions and colour specified by the audio, add it to the
; physics system, give it some movement via kick and twist, also controlled by
; the incoming sound
(define (spawn)
    (push)
        (opacity 1)
        (translate (vector (* (flxrnd) 1) 5 (* (flxrnd) 1)))
        (scale (vector (*(gh 4)10) 0.1 (*(gh 9)10)))
        (shinyness 1)
        (specular (vector 0 0 0))
        (colour (vector (gh 4) (gh 6) (gh 8)))
        (let ((ob (build-cube)))    
            (active-box ob)
            (kick ob (vector (*(gh 5)speed) (*(gh 7)speed) (*(gh 9)speed) ))
            (twist ob (vector (* (- (gh 13) 0.5) rotsp) 
                              (* (- (gh 14) 0.5) rotsp) 
                              (* (- (gh 15) 0.5) rotsp) )))
    (pop))

(clear)
(clear-colour (vector 0 0 1))
; this is a simple way to get the physics system to take ownership of the objects and
; delete them for you, when you've added more than this number to the system
(set-max-physical 200)

(define (run-loop)    
     (spawn))

(every-frame (run-loop))


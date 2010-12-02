#lang racket
; planetarium.ss

(require "fluxus.ss")
(provide 
  dome-pixels
  dome-set-camera-transform 
  dome-set-fov
  dome-lock-camera 
  dome-build 
  dome-pixels
  dome-camera-lag
  dome-setup-main-camera)

;; StartSectionDoc-en
;; planetarium
;; Planetarium rendering code
;; Example:
;; EndSectionDoc 

(define target #f)
(define cameras '())
(define angle 180)
(define target-size 2048)

;; StartFunctionDoc-en
;; dome-set-camera-transform m
;; Returns: primitive-id
;; Description:
;; Get the render target pixels primitive, so you can set the scene
;; up and animate things 'inside' the dome using (with-pixels-renderer)
;; Example:
;; EndFunctionDoc

(define (dome-pixels)
	target)
	
;; StartFunctionDoc-en
;; dome-set-camera-transform m
;; Returns: primitive-id
;; Description:
;; Sets the camera matrix of all the dome cameras
;; Example:
;; EndFunctionDoc

(define (dome-set-camera-transform m)
    (cond ((> (length cameras) 1)
	(let ((width (/ 1 (length cameras))))
    (for-each
      (lambda (camera)
            (current-camera camera)
            (set-fov (/ angle (length cameras)) 0.1 1000)
			; distribute the cameras to render along the texture width
            (viewport (* camera width) 0 (+ width 0.001) 1) 
            (set-camera (mmul (mtranslate (vector 0 0 0)) 
            (mrotate (vector 0 (* (- camera 1) (/ angle (length cameras))) 0)) m)))
     cameras)))
	 (else
	   (set-camera m))))

;; StartFunctionDoc-en
;; dome-camera-set-fov angle near far
;; Returns: primitive-id
;; Description:
;; Sets the fov of the dome cameras (not sure this is right actually...)
;; Example:
;; EndFunctionDoc

(define (dome-set-fov a n f)
 (for-each
      (lambda (camera)
            (current-camera camera)
            (set-fov a n f))
     cameras))

;; StartFunctionDoc-en
;; dome-camera-lag amount
;; Returns: primitive-id
;; Description:
;; Locks all the dome cameras to a primitive
;; Example:
;; EndFunctionDoc
     
(define (dome-lock-camera o)
 (for-each
      (lambda (camera)
            (current-camera camera)
            (lock-camera o))
     cameras))

;; StartFunctionDoc-en
;; dome-camera-lag amount
;; Returns: primitive-id
;; Description:
;; Sets the camera lag for all the dome cameras at once
;; Example:
;; EndFunctionDoc

(define (dome-camera-lag s)
 (for-each
      (lambda (camera)
            (current-camera camera)
            (camera-lag s))
     cameras))

;; StartFunctionDoc-en
;; dome-build num-cameras fov-angle texture-size init-thunk
;; Returns: primitive-id
;; Description:
;; Builds a inverted sphere texture mapped with another scene. The scene is rendered by
;; multiple cameras which are combined to give a view which can be very wide angle. fov-angle
;; sets this size, texture-size sets the resolution of the render target and init-thunk is
;; run in the internal pixels-renderer and is for setting up your scene. (should replace 
;; thunk with a with-dome)
;; Example:
;; EndFunctionDoc

(define (dome-build num-cameras a size)
	(set! angle a)
	(set! target-size size)
    (set! target (with-state
      (translate (vector -20 0 0))
      (build-pixels target-size target-size #t)))
    (with-pixels-renderer target
		(if (eq? num-cameras 3)
        	(set! cameras (append (build-list 2 (lambda (_) (build-camera))) (list 0)))
			(set! cameras (list 0)))
        (dome-set-camera-transform (mident)))
    (let ((p (with-state
            (texture-params 0 '(min linear mag linear)) ; fix for dave's laptop
            (texture (pixels->texture target))
            (hint-unlit)
            (scale -10)
            (build-sphere 30 30))))
		(with-primitive p
			(pdata-map! 
				(lambda (t)
					; rearrange the texture coords so we squash the texture into
					; the correct angle range of the sphere s coordinates
					(vector (min (* (- 1 (vx t)) 2 #;(/ 360 angle)) 1)  (vy t) 0))
				"t"))
			p))

;; StartFunctionDoc-en
;; dome-setup-main-camera
;; Returns: primitive-id
;; Description:
;; Sets up the view of the dome geometry to fit (currently) 
;; the plymouth immersive vision theatre - this should be useful as 
;; an example for other locations.
;; Example:
;; EndFunctionDoc

(define (dome-setup-main-camera w h)
	(ortho)
    (set-screen-size (vector w h))
    (set-camera-update #f) ; turn off the mouse interactive camera
    (set-camera-position (vector 0 0 -7.5)) ; sets ortho-zoom
    (set-camera (mmul 
			; the sphere has to sit at the bottom of the view, 
			; fill the width and have the top 1/4 chopped off
            (mtranslate (vector 0 2.55 -7.5)) 
			; rotate to face the right way, and tilt
            (mrotate (vector -25 90 0)))) ; not sure if the -25 tilt angle is correct
    (set-ortho-zoom -7.5)
    
    (let ((aspect (/ (vx (get-screen-size)) (vy (get-screen-size)))))
        (frustum (* -1 aspect) (* 1 aspect) -1 1)))

#lang scheme
; planetarium.ss

(require fluxus-016/fluxus)
(provide 
  dome-set-camera-transform 
  dome-set-fov
  dome-lock-camera 
  dome-build 
  dome-every-frame
  dome-camera-lag)

(define target #f)
(define cameras '())
(define angle 180)
(define target-size 2048)

(define (dome-set-camera-transform m)
    (cond ((> (length cameras) 1)
	(let ((width (/ 1 (length cameras))))
    (for-each
      (lambda (camera)
            (current-camera camera)
            (set-fov (/ angle (length cameras)) 0.1 1000)
			; distribute the cameras to render along the texture width
            (viewport (* camera width) 0 width 1) 
            (set-camera (mmul (mtranslate (vector 0 0 0)) 
            (mrotate (vector 0 (* (- camera 1) (/ angle (length cameras))) 0)) m)))
     cameras)))
	 (else
	   (set-camera m))))

(define (dome-set-fov a n f)
 (for-each
      (lambda (camera)
            (current-camera camera)
            (set-fov a n f))
     cameras))
     
(define (dome-lock-camera o)
 (for-each
      (lambda (camera)
            (current-camera camera)
            (lock-camera o))
     cameras))

(define (dome-camera-lag s)
 (for-each
      (lambda (camera)
            (current-camera camera)
            (camera-lag s))
     cameras))

(define (dome-build num-cameras a size thunk)
	(set! angle a)
	(set! target-size size)
    (set! target (with-state
      (translate (vector -20 0 0))
      (build-pixels target-size target-size #t)))
    (with-pixels-renderer target
		(if (eq? num-cameras 3)
        	(set! cameras (append (build-list 2 (lambda (_) (build-camera))) (list 0)))
			(set! cameras (list 0)))
        (dome-set-camera-transform (mident))
        (thunk))
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
					(vector (min (* (- 1 (vx t)) (/ 360 angle)) 1)  (vy t) 0))
				"t"))
			p))

(define (dome-every-frame thunk)
    (spawn-task 
      (lambda () (with-pixels-renderer target
         (thunk)))
       'dome))

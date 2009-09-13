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

(define target-size 2048)

(define (dome-set-camera-transform m)
    (cond ((> (length cameras) 1)
    (for-each
      (lambda (camera)
            (current-camera camera)
            (set-fov 90 0.1 1000)
            (viewport (* camera 0.25) 0 0.25 1)
            (set-camera (mmul (mtranslate (vector 0 0 0)) 
            (mrotate (vector 0 (* (- camera 1) 90) 0)) m)))
     cameras))
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

(define (dome-build num-cameras thunk)
    (set! target (with-state
      (translate (vector -20 0 0))
      (build-pixels target-size target-size #t)))
    (with-pixels-renderer target
		(if (eq? num-cameras 3)
        	(set! cameras (append (build-list 2 (lambda (_) (build-camera))) (list 0)))
			(set! cameras (list 0)))
        (dome-set-camera-transform (mident))
        (thunk)
        #;(with-state
            (scale 1000)
            (hint-unlit)
            (backfacecull 0)
            (colour 1)
            (texture (load-texture "textures/grid.png"))
            (build-sphere 20 20)))
    (let ((p (with-state
            (texture-params 0 '(min linear mag linear))
            (texture (pixels->texture target))
            (hint-unlit)
            (scale -10)
            (build-sphere 30 30))))
		(with-primitive p
			(pdata-map! 
				(lambda (t)
					(vector (vx t) (- (vy t)) 0))
				"t"))
			p))

(define (dome-every-frame thunk)
    (spawn-task 
      (lambda () (with-pixels-renderer target
         (thunk)))
       'dome))

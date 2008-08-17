(clear)
(viewport 0 0.5 0.5 0.5)

(define cam2 (build-camera))
(current-camera cam2)
(viewport 0.5 0 0.5 1)

(define cam3 (build-camera))
(current-camera cam3)
(set-camera (mmul (mtranslate (vector 0 0 -5))
        (mrotate (vector 0 45 0))))
(viewport 0 0 0.5 0.5)


(current-camera 0)

(define c (with-state
        (hint-cull-ccw)
        (hint-unlit)
        (hint-wire)
        (line-width 2)
        (colour (vector 0.4 0.3 0.2))
        (wire-colour (vector 0 0 0))
        (scale 10)    
        (build-cube)))

(define p (with-state
        (scale 3)
        (load-primitive "widget.obj")))

(every-frame
    (with-primitive p
        (rotate (vector 0 1 0))))
;; fluxus-artkp example
;; detecting simple id-based markers

(require fluxus-018/fluxus-video)
(require fluxus-018/fluxus-artkp)

(clear)
(camera-clear-cache)
(camera-list-devices)

; check the device id's in the console and change the first parameter of
; camera-init to the id
(define cam (camera-init 0 640 480))

; init the ar system with the camera resolution and the camera parameter file
(ar-init (camera-width cam) (camera-height cam) "data/camera_para.dat")

; enable automatic threshold calculation to adapt to different lighting conditions
(ar-auto-threshold #t)

(set-projection-transform (ar-get-projection-matrix))
(set-camera-transform (mident))

; display the camera texture
(let ([p (build-image cam #(0 0) (get-screen-size))]
        [tcoords (camera-tcoords cam)])
    (with-primitive p
        (pdata-index-map!
            (lambda (i t)
                (list-ref tcoords (remainder i 4)))
            "t")))

(define (mainloop)
    ; get next frame from camera
    (camera-update cam)
    ; detect the markers in the image 
    (let ([marker-count (ar-detect (camera-imgptr cam))])
        ; get the modelview matrix of each marker and draw
        ; a cube on the marker
        (printf "detected markers: ~a~n" marker-count)
        (for ([i (in-range marker-count)])
            (let ([m (ar-get-modelview-matrix i)]
                  [id (ar-get-id i)]
                  [cf (ar-get-confidence i)])
                (printf "index:~a id:~a cf:~a ~n" i id cf)
                (when id
                    (with-state
                        (hint-none)
                        (hint-wire)
                        (backfacecull 0)
                        (line-width 5)
                        (concat m)
                        ; scale it up to pattern width (80mm)
                        (scale 80)
                        ; move the cube center up by half to match the marker
                        (translate #(0 0 .5))
                        (draw-cube)))))))

(every-frame (mainloop))


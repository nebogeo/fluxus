(require fluxus-018/fluxus-video)

(clear)
(camera-clear-cache)
(camera-list-devices)

; check the device id's in the console and change the first parameter of
; camera-init to the id
(define vt (camera-init 0 320 240))

(let ([p (build-plane)]
      [tcoords (camera-tcoords vt)])
    (with-primitive p
        (texture vt)
        (pdata-index-map!
            (lambda (i t)
                (list-ref tcoords (remainder i 4)))
            "t")))

(every-frame (camera-update vt))


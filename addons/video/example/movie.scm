(require fluxus-018/fluxus-video)

(clear)
(video-clear-cache)

; use full path on linux
(define vt (video-load "/path/to/movie"))
(video-play vt)

(let ([p (build-plane)]
      [tcoords (video-tcoords vt)])
    (with-primitive p
        (texture vt)
        (pdata-index-map!
            (lambda (i t)
                (list-ref tcoords (remainder i 4)))
            "t")))

(every-frame (video-update vt))


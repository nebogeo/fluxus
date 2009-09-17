;; fluxus scratchpad effects example
;; press f9 to switch on and off scratchpad effects
;;
;; set the size parameter of the effect to other than 0 to switch it on

(spawn-task
    (lambda ()
        ;; jiggle
        (set! fluxus-scratchpad-effect-jiggle-size 0)

        ;; wave
        (set! fluxus-scratchpad-effect-wave-size 0)
        (set! fluxus-scratchpad-effect-wave-wavelength 2.5)
        (set! fluxus-scratchpad-effect-wave-speed 2.0)

        ;; ripple
        (set! fluxus-scratchpad-effect-ripple-size 0)
        (set! fluxus-scratchpad-effect-ripple-center-x (mouse-x))
        (set! fluxus-scratchpad-effect-ripple-center-y (mouse-y))
        (set! fluxus-scratchpad-effect-ripple-wavelength 1)
        (set! fluxus-scratchpad-effect-ripple-speed 2.0)

        ;; swirl
        (set! fluxus-scratchpad-effect-swirl-size 15)
        (set! fluxus-scratchpad-effect-swirl-center-x (mouse-x))
        (set! fluxus-scratchpad-effect-swirl-center-y (mouse-y))
        (set! fluxus-scratchpad-effect-swirl-rotation (* 10 (sin (time)))))

    'scratchpad-effect-task)



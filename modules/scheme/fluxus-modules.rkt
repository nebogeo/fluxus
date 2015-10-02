;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

(module fluxus racket

; for the moment, comment the top require/provide out and uncomment the
; lower when installing a static build of the fluxus modules

(require (rename-in "fluxus-engine.rkt"
					(shader-set! shader-list-set!))
         "fluxus-audio.rkt"
         "fluxus-osc.rkt"
         "fluxus-midi.rkt"
         "fluxus-openal.rkt"
         ;"fluxus-video.rkt"
)

(provide
 (all-from-out "fluxus-engine.rkt")
 (all-from-out "fluxus-audio.rkt")
 (all-from-out "fluxus-osc.rkt")
 (all-from-out "fluxus-midi.rkt")
 (all-from-out "fluxus-openal.rkt")
 ;(all-from-out "fluxus-video.rkt")

)

#;(require (rename-in 'fluxus-engine
					(shader-set! shader-list-set!))
         'fluxus-audio
         'fluxus-osc
         'fluxus-midi)

#;(provide
 (all-from-out 'fluxus-engine)
 (all-from-out 'fluxus-audio)
 (all-from-out 'fluxus-osc)
 (all-from-out 'fluxus-midi))

)

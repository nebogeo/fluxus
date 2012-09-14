;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

#lang racket/base

; for the moment, comment the top require/provide out and uncomment the 
; lower when installing a static build of the fluxus modules

(require (rename-in "fluxus-engine.ss"
					(shader-set! shader-list-set!))
         "fluxus-audio.ss"
         "fluxus-osc.ss"
         "fluxus-midi.ss"
	 "fluxus-openal.ss"
         "fluxus-video.ss"
)
		 	 
(provide 
 (all-from-out "fluxus-engine.ss")
 (all-from-out "fluxus-audio.ss")
 (all-from-out "fluxus-osc.ss")
 (all-from-out "fluxus-midi.ss")
 (all-from-out "fluxus-openal.ss")
 (all-from-out "fluxus-video.ss")

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

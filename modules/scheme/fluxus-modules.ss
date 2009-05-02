;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

#lang scheme/base

; for the moment, comment the top require/proved out and uncomment the 
; lower when installing a static build of the fluxus modules

(require "fluxus-engine.ss"
         "fluxus-audio.ss"
         "fluxus-osc.ss"
         "fluxus-midi.ss")
		 	 
(provide 
 (all-from-out "fluxus-engine.ss")
 (all-from-out "fluxus-audio.ss")
 (all-from-out "fluxus-osc.ss")
 (all-from-out "fluxus-midi.ss"))

#;(require 'fluxus-engine
         'fluxus-audio
         'fluxus-osc
         'fluxus-midi)
		 	 
#;(provide 
 (all-from-out 'fluxus-engine)
 (all-from-out 'fluxus-audio)
 (all-from-out 'fluxus-osc)
 (all-from-out 'fluxus-midi))

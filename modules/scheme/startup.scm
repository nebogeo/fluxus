; .-----..-.    .-. .-..-.  .-..-. .-..-----.
; | .---'| |    | | | || |  | || | | || .---'
; | `---.| |    | | | | \ \/ / | | | || `---.
; | .---'| |    | | | | / /\ \ | | | |`---. |
; | |    | `---.| `-' || |  | || `-' |.---' |
; `-'    `-----'`-----'`-'  `-'`-----'`-----'
; (c) 2007 dave griffiths dave@pawfal.org GPL

; boot up the fluxus scratchpad
(load (string-append fluxus-collects-location "/fluxus-"  
	fluxus-version "/scratchpad-boot.scm"))
; the line above ensures the correct bootup script 
; is called for the right version of fluxus

; you can add your own commands here which will be run on startup
(start-audio "alsa_pcm:capture_1" 1024 44100)

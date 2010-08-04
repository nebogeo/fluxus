;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

#lang racket/base
(require scheme/class)
(require "fluxus.ss")
(require "time.ss")

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; a client for itchy - a multitimbral lo tech rave synth and sampler

(provide itchy-client%)

(define itchy-client% 
  (class object%
    (init-field 
     (itchy-osc-url "")
	 (sample-location ""))
     
     (field
      (current-osc-url 0)
      (voice-count 7)
      (sample-id 0))
     
     (define/public (init)
       (osc-dest itchy-osc-url)
       (osc-send "/clear" "" '())
       ;(osc-send "/debug" "i" '(1))
       (osc-send "/setclock" "" '())
       (osc-send "/addsearchpath" "s" (list sample-location))
       (voice-volume 3)
       (sample-volume 3))
     
     
     (define/public (osc-dest new)
       (cond 
         (#t ;(not (equal? current-osc-url new))
          (set! current-osc-url new)
          (osc-destination current-osc-url))))
     
     (define/public (voice id type keyvalue)
       (define build-argsstr
         (lambda (keyvalue str)
           (set! str (string-append str "sf"))
           (if (eq? (cdr (cdr keyvalue)) '())
               str
               (build-argsstr (cdr (cdr keyvalue)) str))))
       
       (osc-send "/addinstrument" "is" (list id type))
       (set! keyvalue (cons id keyvalue))
       (set! keyvalue (cons "map" keyvalue))
       (osc-send "/modify" 
                 (build-argsstr keyvalue "i") 
                 (append (list id) keyvalue)))
     
     ; set voice volume
     (define/public (voice-volume vol)
       (osc-send "/globalvolume" "f" (list vol)))
     
     (define/public (load-scale-lut filename)
       (let ((f (open-input-file filename)))
         (set! scale-lut (read f))
         (close-input-port f)))
     
     (define/public (play notetime note voice)
       (let ((timestamp (time->timestamp notetime)))
         (let ((freq (list-ref scale-lut (modulo note 32))))
           ;(display "playing ")(display (* 0.25 (list-ref scale-lut note)))(newline)
           ;(display "playing ")(display voice)(newline)
           (osc-send "/play" "iiiffffi" 
                     (list (vector-ref timestamp 0) 
                           (vector-ref timestamp 1) 
                           (+ 1 (modulo (- voice 1) voice-count)) freq 0 1 0.5 79)))))
     
     ; load a sample into a voice
     (define/public (sample id file)
       (set! sample-id (+ sample-id 1))
       (osc-send "/addtoqueue" "is" (list sample-id file))
       (osc-send "/loadqueue" "" '())
       (osc-send "/map" "ii" (list sample-id id)))
     
     ; set global properties for a sample
     (define/public (sample-globals id vol freq pan)
       (osc-send "/setglobals" "ifff" (list id vol freq pan)))
     
     ; sets the global sampler volume
     (define/public (sample-volume vol)
       (osc-send "/globalvolume" "f" (list vol)))
     
     ; sets the global pitch of the sampler
     (define/public (sample-pitch pitch)
       (osc-send "/globalpitch" "f" (list pitch)))
     
     (define/public (sample-unmap)
       (osc-send "/unmapall" "" '()))
     
     ; load a load of samples in one go from a directory specified and
     ; assign them to one voice for pitch based percussion style use
     (define/public (samples id dir)    
       (define get-samples
         (lambda (d l)
           (cond ((null? d)
                  l)
                 (else
                  (let ((ret (path->string (car d))))
                    (when (and (> (string-length ret) 4)
                               (or (string=? (substring ret (- (string-length ret) 4)) ".wav")
                                   (string=? (substring ret (- (string-length ret) 4)) ".WAV")))
                      (set! l (append l (list ret))))
                    (get-samples (cdr d) l))))))
       
       (define load-list
         (lambda (id dir l)
           (set! sample-id (+ sample-id 1))
           (osc-send "/addtoqueue" "is" (list sample-id (string-append dir "/" (car l))))
           (osc-send "/map" "ii" (list sample-id id))
           (if (eq? (cdr l) '())
               0
               (load-list id dir (cdr l)))))
       
       (osc-send "/unmap" "i" (list id))
       (load-list id dir (get-samples (directory-list (string-append sample-location dir)) '()))
       (osc-send "/loadqueue" "" '()))

    ; sort this mess out - load directly from scala
    (define scale-lut (list 58.2705 61.7354 65.4064 69.2957 73.4162 77.7817 
                        82.4069 87.3071 92.4986 97.9989 103.826 110 116.541 123.471 130.813 138.591 
                        146.832 155.563 164.814 174.614 184.997 195.998 207.652 220 233.082 246.942 
                        261.626 277.183 293.665 311.127 329.628 349.228 369.994 391.995 415.305 440 
                        466.164 493.883 523.251 554.365 587.33 622.254 659.255 698.456 739.989 783.991 
                        830.609 880 932.328 987.767 1046.5 1108.73 1174.66 1244.51 1318.51 1396.91 
                        1479.98 1567.98 1661.22 1760 1864.66 1975.53 2093 2217.46 2349.32 2489.02 
                        2637.02 2793.83 2959.96 3135.96 3322.44 3520 3729.31 3951.07 4186.01 4434.92 
                        4698.64 4978.03 5274.04 5587.65 5919.91 6271.93 6644.88 7040 7458.62 7902.13 
                        8372.02 8869.84 9397.27 9956.06 10548.1 11175.3 11839.8 12543.9 13289.8 14080 
                        14917.2 15804.3 16744 17739.7 18794.5 19912.1 21096.2 22350.6 23679.6 25087.7 
                        26579.5 28160 29834.5 31608.5 33488.1 35479.4 37589.1 39824.3 42192.3 44701.2 
                        47359.3 50175.4 53159 56320))    

;(define scale-lut '(58.2705 61.7354 65.4064 69.2957 73.4162 77.7817 82.4069 87.3071 92.4986 97.9989 103.826 
;110 116.541 123.471 130.813 138.591 146.832 155.563 164.814 174.614 184.997 195.998 207.652 220 233.082 
;246.942 261.626 277.183 293.665 311.127 329.628 349.228 369.994 391.995 415.305 440 466.164 493.883 523.251 
;554.365 587.33 622.254 659.255 698.456 739.989 783.991 830.609 880 932.328 987.767 1046.5 1108.73 1174.66 
;1244.51 1318.51 1396.91 1479.98 1567.98 1661.22 1760 1864.66 1975.53 2093 2217.46 2349.32 2489.02 2637.02 
;2793.83 2959.96 3135.96 3322.44 3520 3729.31 3951.07 4186.01 4434.92 4698.64 4978.03 5274.04 5587.65 
;5919.91 6271.93 6644.88 7040 7458.62 7902.13 8372.02 8869.84 9397.27 9956.06 10548.1 11175.3 11839.8 
;12543.9 13289.8 14080 14917.2 15804.3 16744 17739.7 18794.5 19912.1 21096.2 22350.6 23679.6 25087.7 26579.5 
;28160 29834.5 31608.5 33488.1 35479.4 37589.1 39824.3 42192.3 44701.2 47359.3 50175.4 53159 56320 58.2705 
;61.7354 65.4064 69.2957 73.4162 77.7817 82.4069 87.3071 92.4986 97.9989 103.826 110 116.541 123.471 130.813 
;138.591 146.832 155.563 164.814 174.614 184.997 195.998 207.652 220 233.082 246.942 261.626 277.183 293.665 
;311.127 329.628 349.228 369.994 391.995 415.305 440 466.164 493.883 523.251 554.365 587.33 622.254 659.255 
;698.456 739.989 783.991 830.609 880 932.328 987.767 1046.5 1108.73 1174.66 1244.51 1318.51 1396.91 1479.98 
;1567.98 1661.22 1760 1864.66 1975.53 2093 2217.46 2349.32 2489.02 2637.02 2793.83 2959.96 3135.96 3322.44 
;3520 3729.31 3951.07 4186.01 4434.92 4698.64 4978.03 5274.04 5587.65 5919.91 6271.93 6644.88 7040 7458.62 
;7902.13 8372.02 8869.84 9397.27 9956.06 10548.1 11175.3 11839.8 12543.9 13289.8 14080 14917.2 15804.3 16744 
;17739.7 18794.5 19912.1 21096.2 22350.6 23679.6 25087.7 26579.5 28160 29834.5 31608.5 33488.1 35479.4 
;37589.1 39824.3 42192.3 44701.2 47359.3 50175.4 53159 56320 58.2705 61.7354 65.4064 69.2957 73.4162 77.7817 
;82.4069 87.3071 92.4986 97.9989 103.826 110 116.541 123.471 130.813 138.591))

     (super-new)
     (init)))

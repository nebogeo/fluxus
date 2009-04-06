;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

; an experimental non-determinsistic synth

;; StartSectionDoc-en
;; fluxa
;; Fluxa is the fluxus audio synth
;; Example:
;; EndSectionDoc 
#lang scheme/base

(require fluxus-016/scratchpad 
	     fluxus-016/tasks 
	     "fluxus-osc.ss" 
	     "fluxus-engine.ss" 
	      scheme/list)
(provide
 play play-now seq clock-map clock-split volume pan max-synths note searchpath reset eq comp
 sine saw tri squ white pink adsr add sub mul div pow mooglp moogbp mooghp formant sample
 crush distort klip echo reload zmod sync-tempo sync-clock fluxa-init fluxa-debug set-global-offset)

(define time-offset 0.0) 
(define sync-offset 0.01)
(define nm-searchpath "/home/dave/noiz/nm/")

(define TERMINAL 0) (define SINE 1) (define SAW 2) (define TRI 3) (define SQU 4) 
(define WHITE 5) (define PINK 6) (define ADSR 7) (define ADD 8) (define SUB 9) 
(define MUL 10) (define DIV 11) (define POW 12) (define MOOGLP 13) (define MOOGBP 14) 
(define MOOGHP 15) (define FORMANT 16) (define SAMPLE 17) (define CRUSH 18) 
(define DISTORT 19) (define CLIP 20) (define ECHO 21)

(define (fluxa-init)
  (osc-destination "osc.udp://127.0.0.1:4004")
  (osc-source "4444")
  (osc-send "/setclock" "" '())
  (searchpath nm-searchpath)
  (spawn-task go-flux 'fluxa-update-task))

;------------------------------
; infrastructure

(define current-id 0)

(define (new-id)
  (let ((ret (+ current-id 1)))
    (set! current-id ret)
    ret))  

(define-struct node (id))

(define (get-node-id v)
  (cond ((node? v)
         (node-id v))
        (else
         (let ((id (new-id)))
           (osc-send "/create" "iif" (list id TERMINAL v))
           id))))

(define (make-args id operands)
  (let ((index -1))
    (foldl
     (lambda (a l)
       (set! index (+ index 1))
       (append l (list id index (get-node-id a))))
     '()
     operands)))

(define (make-format operands)
  (make-string (* 3 (length operands)) #\i))

(define (operator op operands)
  (let ((id (new-id)))
    (osc-send "/create" "ii" (list id op))
    (osc-send "/connect" 
              (make-format operands)
              (make-args id operands))
    (make-node id)))

(define current-sample-id 0)
(define samples '())

(define (reload)
  (set! samples '()))

(define (get-sample-id filename)
  (let ((t (assoc filename samples)))
    (cond ((list? t) (cadr t))
          (else       			
           (osc-send "/addtoqueue" "is" (list current-sample-id filename))
           (set! samples (cons (list filename current-sample-id) samples))          
           (set! current-sample-id (+ current-sample-id 1))
           (- current-sample-id 1)))))

;------------------------------
; synthesis	

;; StartFunctionDoc-en
;; sine frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a sine wave generator node
;; Example:
;; (play-now (mul (sine 440) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (sine a)
  (operator SINE (list a)))

;; StartFunctionDoc-en
;; saw frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a saw wave generator node
;; Example:
;; (play-now (mul (saw 440) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (saw a)
  (operator SAW (list a)))

;; StartFunctionDoc-en
;; tri frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a triangle wave generator node
;; Example:
;; (play-now (mul (tri 440) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (tri a)
  (operator TRI (list a)))

;; StartFunctionDoc-en
;; squ frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a square wave generator node
;; Example:
;; (play-now (mul (squ 440) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (squ a)
  (operator SQU (list a)))

;; StartFunctionDoc-en
;; white frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a white noise generator node
;; Example:
;; (play-now (mul (white 5) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (white a)
  (operator WHITE (list a)))

;; StartFunctionDoc-en
;; pink frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a pink noise generator node
;; Example:
;; (play-now (mul (pink 5) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (pink a)
  (operator PINK (list a)))

;; StartFunctionDoc-en
;; add number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - adds two signals together
;; Example:
;; (play-now (mul (add (sine 440) (sine 220)) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (add a b)
  (operator ADD (list a b)))

;; StartFunctionDoc-en
;; sub number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - subtracts two signals
;; Example:
;; (play-now (mul (sub (sine 440) (sine 220)) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (sub a b)
  (operator SUB (list a b)))

;; StartFunctionDoc-en
;; mul number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - multiplies two signals
;; Example:
;; (play-now (mul (mul (sine 440) (sine 220)) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (mul a b)
  (operator MUL (list a b)))

;; StartFunctionDoc-en
;; mul number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - multiplies two signals
;; Example:
;; (play-now (mul (div (sine 440) 2) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (div a b)
  (operator DIV (list a b)))

;; StartFunctionDoc-en
;; pow number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - produces a signal raised to the power of another
;; Example:
;; (play-now (mul (pow (adsr 0 0.1 0 0) 10) (sine 440)))
;; EndFunctionDoc    

(define (pow a b)
  (operator POW (list a b)))

;; StartFunctionDoc-en
;; adsr attack-number-or-node decay-number-or-node sustain-number-or-node release-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates an envelope generator node
;; Example:
;; (play-now (mul (sine 440) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (adsr a d s r)
  (operator ADSR (list a d s r)))

;; StartFunctionDoc-en
;; mooglp signal-node cutoff-number-or-node resonance-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates an low pass moog filter node
;; Example:
;; (play-now (mul (mooglp (squ 440) 0.1 0.4) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (mooglp in cutoff resonance)
  (operator MOOGLP (list in cutoff resonance)))

;; StartFunctionDoc-en
;; moogbp signal-node cutoff-number-or-node resonance-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates an band pass moog filter node
;; Example:
;; (play-now (mul (moogbp (squ 440) 0.1 0.4) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (moogbp in cutoff resonance)
  (operator MOOGBP (list in cutoff resonance)))

;; StartFunctionDoc-en
;; mooghp signal-node cutoff-number-or-node resonance-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates an high pass moog filter node
;; Example:
;; (play-now (mul (mooghp (squ 440) 0.1 0.4) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (mooghp in cutoff resonance)
  (operator MOOGHP (list in cutoff resonance)))

;; StartFunctionDoc-en
;; formant signal-node cutoff-number-or-node resonance-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a formant filter node
;; Example:
;; (play-now (mul (formant (squ 440) 0.1 0.4) (asdr 0.1 0.1 0 0)))
;; EndFunctionDoc    

(define (formant in cutoff resonance)
  (operator FORMANT (list in cutoff resonance)))

(define (sample filename freq)
  (operator SAMPLE (list (get-sample-id filename) freq)))

(define (crush in freq bits)
  (operator CRUSH (list in bits freq)))

(define (distort in amount)
  (operator DISTORT (list in amount)))

(define (klip in amount)
  (operator CLIP (list in amount)))

(define (echo in delaytime feedback)
  (operator ECHO (list in delaytime feedback)))


(define (play time node (f '()))
  (let ((time (time->timestamp time)))
    (osc-send "/play" "iii" (list (vector-ref time 0) 
                                  (vector-ref time 1) 
                                  (node-id node))))
  (when (not (null? f))
   	(spawn-timed-task time f)))

(define (play-now node)
  (osc-send "/play" "iii" (list 0 0 (node-id node))))

;------------------------------
; global controls

(define (fluxa-debug v)
  (osc-send "/debug" "i" (list v)))

(define (volume v)
  (osc-send "/globalvolume" "f" (list v)))

(define (pan v)
  (osc-send "/pan" "f" (list v)))

(define (max-synths s)
  (osc-send "/maxsynths" "i" (list s)))

(define (searchpath path)
  (osc-send "/addsearchpath" "s" (list path)))

(define (eq l m h)
  (osc-send "/eq" "fff" (list l m h)))

(define (comp a r t s)
  (osc-send "/comp" "ffff" (list a r t s)))

(define (note n)
  (list-ref scale-lut (modulo n (length scale-lut))))

(define (reset)
  (osc-send "/reset" "" '()))

;------------------------------
; sundry items

;; just intonation (erm I think...)
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

; converts from UTC time to get a 64bit NTP timestamp
(define (time->timestamp time)
  ; january 1972 UTC -> january 1900 NTP era (overflow in 2036...)
  (let ((adjusted (+ time 2208988800L0))) 
    ; floor the time for the seconds
    (let ((seconds (inexact->exact (floor adjusted)))) 
      ; get the remainder and scale to max unsigned int for the fraction of the second
      (let ((frac (inexact->exact (floor (* (- adjusted seconds) 4294967295))))) 
        (vector seconds frac)))))

; ... and back the other way
(define (timestamp->time timestamp)
  (+ (- (vector-ref timestamp 0) 2208988800L0) (/ (vector-ref timestamp 1) 4294967295.0)))

;------------------------------
; sequencing forms

(define-syntax clock-map
  (syntax-rules ()
    ((_ proc clock data ...)
     (proc (list-ref data (modulo clock (length data))) ...))))

(define-syntax clock-split
  (syntax-rules ()
    ((_ clock div proc ...)
     (clock-split-imp clock div (list proc ...)))))

(define (clock-split-imp clock div proclist)
  ((list-ref proclist 
             (modulo (quotient clock div) 
                     (length proclist)))))

(define (zmod clock n)
  (zero? (modulo clock n)))

(define (seq p)
  (set! proc p))

(define proc
  (lambda (time clock)
    0))

;---------------------------------------
; fluxus implementation 

(define logical-time (time-now))
(define clock 0)
(define next-load-queue (time-now))
(define tempo 0.1)
(define sync-tempo 0.5)
(define sync-clock 0)
(define bpb 4)
(define global-offset 0)

(define (set-global-offset s)
	(set! global-offset s))

; figures out the offset to the nearest tick    
(define (calc-offset timenow synctime tick)
  (let ((p (/ (- synctime timenow) tick)))
    (let ((f (- p (floor p))))
      (if (< f 0.5)
          (* f tick)
          (- (* (- 1 f) tick))))))
		  
(define (fluxa-error-handler n)
  (printf "fluxa error:~a~n" n))
  
(define (go-flux)
  ; check for sync messages 
  (cond ((osc-msg "/sync")
         (set! sync-tempo (* (/ 1 (osc 3)) 60))
         (set! bpb (osc 2))
         (let* ((sync-time (+ sync-offset (timestamp->time (vector (osc 0) (osc 1)))))
                (offset (calc-offset logical-time sync-time sync-tempo)))
           (printf "time offset: ~a~n" offset)
           (set! logical-time (+ logical-time offset global-offset))
           (set! sync-clock 0))))
  
  (cond ((> (- (time-now) logical-time) 3)
         (set! logical-time (time-now))))
  
  ; time for an update?
  (cond ((> (time-now) logical-time)	
  		 ; todo: fall back on last thunk if there is an error
		 (with-handlers ([(lambda (x) #t) fluxa-error-handler])
	         (set! tempo (proc (+ logical-time (* bpb tempo)) clock)))   
         (set! logical-time (+ logical-time tempo))
         (set! clock (+ clock 1))
         (set! sync-clock (+ sync-clock 1))))
    
  ; send a loadqueue request every 5 seconds
  (cond ((> (time-now) next-load-queue)	   
         (osc-send "/loadqueue" "" '())
         (set! next-load-queue (+ next-load-queue 5)))))

(fluxa-init)

;---------------------------------------
; drscheme implementation

#;(define (go)
    (define (loop tempo time clock)
      (sleep 0.01)
      (cond 
        ((> (time-now) time)
         (proc (+ time time-offset) clock)
         (set! time (+ time tempo))
         (set! clock (+ clock 1))))
      (loop tempo time clock))
    
    (display "going...")(newline)
    (osc-send "/setclock" "" '())
    (loop tempo (time-now) 0))

;(define thr (thread go))





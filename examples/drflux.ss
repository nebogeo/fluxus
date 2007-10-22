(define fluxus-collects-location "/usr/local/lib/plt/collects/")
(define fluxus-version "0.13")
(load (string-append fluxus-collects-location "fluxus-" 
                     fluxus-version "/scratchpad-boot.scm"))

(module drflux mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "scratchpad.ss" "fluxus-0.13")
           (lib "gl.ss" "sgl")
           (prefix gl- (lib "sgl.ss" "sgl"))
           fluxus-engine
           fluxus-audio)
  
  (define fluxus-canvas%
    (class* canvas% ()
      (inherit with-gl-context swap-gl-buffers)
      
      (define/override (on-paint)
        (with-gl-context
         (lambda ()           
           ; don't *need* to call this every frame, but 
           ; it doesn't hurt - just need to call it inside
           ; the gl context
           (fluxus-init)
           
           (fluxus-frame-callback)
           
           ; swap the buffers 
           (swap-gl-buffers)
           (super on-paint))))
      
      (define/override (on-size width height)
        (with-gl-context
         (lambda () 0)))
      
      (define/override (on-char event)
        (display (send event get-key-code))(newline)
        ;(display (send event get-key-release-code))(newline)
        (if (equal? 'release (send event get-key-code))
            (fluxus-input-release-callback (send event get-key-code) 0 0 0 0 0 0))
            (fluxus-input-callback (send event get-key-code) 0 0 0 0 0 0))
        
      
      (super-instantiate () (style '(gl)))))
  
  ; make and show the window and canvas 
  (define frame (instantiate frame% ("fluxus")))
  (define fluxus-canvas (instantiate fluxus-canvas% (frame) (min-width 640) (min-height 400)))
  (send frame show #t)
  
  (define (loop)
    (send fluxus-canvas on-paint)
    (loop))
  
  (define thr (thread loop))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require drflux)

(define (test n)
  (translate (vector 0 1 0))
  (rotate (vector 0 
                  (* 45 (cos (+ (time) n))) 
                  (* 45 (sin (+ (* (time) 1.2) n)))))
  
  (if (key-pressed " ")
      (draw-sphere)
      (draw-cube))
  
  (if (zero? n)
      0
      (test (- n 1))))
(fluxus-init)
(show-fps 1)
(blur 0.001)
(every-frame (test 100))









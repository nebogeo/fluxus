; This is a script explaining how to use fluxus inside drscheme.
; It's a bit of a mess at the moment, but we can make a gl window 
; using plt's widget classes, and put a fluxus renderer inside it 
; to use as normal (more or less). It can then be run from inside
; drscheme, in order to use a "real proper" editor, with visual 
; debugging etc - but keeping the ability to hit F5 and see a 
; render happen.
;
; The "more or less" bit needs more thinking and work on the 
; plt scheme side, for instance:
;
; * The scene is only redrawn on an paint event (need to hook into idle,
;   or call on-paint from a thread?) This has implications on sorting
;   out a general livecoding system for plt scheme...
;
; * No scratchpad functions present - you can only use the low level
;   commands in the fluxus binary modules. Things you have to implement
;   yourself include:
;
;   - Keyboard/mouse input. there are functions to handle events in 
;     mred already - it would be nice to get a compatibility
;     layer that passes events from mred to the fluxus commands 
; 
;   - Camera controls - again, we can copy code from the fluxus scratch
;     pad to do this, but it will need hooking into mred's input events.
;
; * Probably other things I haven't remembered, but this will improve 
;   in future versions.


; load the gl functions
(require 
 (lib "gl.ss" "sgl")
 (prefix gl- (lib "sgl.ss" "sgl")))

; load the fluxus renderer extension
(load-extension (string-append 
                 (path->string (car (cdr (current-library-collection-paths))))
                 "/fluxus-0.13/extensions/fluxus-engine.so"))

(require fluxus-engine)
(clear-engine)

(define ob '())

; a simple hack of the reflection map example...
(define (setup)
  (clear-colour (vector 0 0 0))
  
  (let ((l (make-light 'point 'free)))
    (light-position l (vector 10 50 10)))
  
  (shinyness 80)
  (specular (vector 1 1 1))
  (line-width 4)
  (texture (force-load-texture "textures/refmap.png"))
  
  (push)
  (hint-unlit)
  (rotate (vector 180 90 0))
  (scale (vector -10 -10 -10))
  (build-sphere 6 6)
  (pop)
  
  (colour (vector 0.5 0.5 1))
  (scale (vector 3 3 3))
  (set! ob (build-nurbs-sphere 20 20))
  (grab ob)
  ; more reference geometry for the deformation
  (pdata-copy "p" "pref")
  (ungrab))

; some sinewave deformation with time
(define (deform n)
  (let ((v (vector (* 1 (sin (+ (flxtime) (* (vector-ref (pdata-ref "pref" n) 1) 5.4)))) 0 0)))
    (set! v (vmul v (* (sin (flxtime)) 0.5)))
    (pdata-set! "p" n (vadd v (pdata-ref "pref" n))))
  (if (< n 0)
      0
      (deform (- n 1))))    


; take the eye and normal vectors and return the texture coordinates of the 
; reflection vector, calculated by converting them into a spherical lookup
; this is a bad quality, but fast - and looks like it might be buggy
(define reflect
  (lambda (eye normal)
    (let ((refl (vmul (vsub eye normal) (vdot eye normal)))) ; reflection vec
      (vector (+ (/ (vector-ref refl 0) 2) 0.5) ; s coord
              (+ (/ (vector-ref refl 1) 2) 0.5) 0)))) ; t coord

; this is the interesting part, the facing ratio is the dot product of the direction we are looking 
; at the vertex from, and the normal of the vertex - where all vectors are normalised. the complex bit
; is getting the incident direction, from the camera space transform (see below) and the vertex position
; in worldspace.
(define (toon n camerapos obpos)
  (let ((v (vadd obpos (pdata-ref "p" n))))             ; find the vertex in worldspace 
    (let ((i (vnormalise (vsub v camerapos))))          ; incident direction (normalised)
      (pdata-set! "t" n (reflect i (pdata-ref "n" n))))) ; set s to the facing ratio (i dot n) 
  (if (< n 0)
      0
      (toon (- n 1) camerapos obpos)))    

(define (myrender)
  (grab ob)
  (deform (pdata-size))
  (recalc-normals 1)
  (toon (pdata-size)
        ; transforming a vector gets that vector "into" the space of the transform, so 0,0,0 in camera
        ; space is the camera position...
        (vtransform (vector 0 0 0) (get-camera)) ; gets the eye position
        ; and 0,0,0 in object space is the object position
        (vtransform (vector 0 0 0) (get-transform))) ; gets the object position
  (ungrab))


; now this is the plt gui code (using mred)
(define fluxus-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (glClearColor 0.0 0.0 0.0 0.0)
         (gl-clear 'color-buffer-bit 'depth-buffer-bit)
         
         ; don't *need* to call this every frame, but 
         ; it doesn't hurt - just need to call it inside
         ; the gl context
         (fluxus-init)
                  
         ; if it's the first time, setup the scene
         (if (null? ob) (setup))
         
         (myrender)
         
         ; render stuff
         (fluxus-render)
         
         ; swap the buffers 
         (swap-gl-buffers)
         (super on-paint))))
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda () 0)))
    
    (super-instantiate () (style '(gl)))))

; make and show the window and canvas 
(define frame (instantiate frame% ("fluxus in mred")))
(define fluxus-canvas (instantiate fluxus-canvas% (frame) (min-width 640) (min-height 400)))
(send frame show #t)

(define (loop)
  (send fluxus-canvas on-paint)
  (loop))

(define thr (thread loop))
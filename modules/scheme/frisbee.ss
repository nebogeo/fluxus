; Copyright (C) 2007 Dave Griffiths
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; StartSectionDoc-en
;; frisbee
;; Frisbee is an experimental high level game engine written for FrTime, a functional reactive
;; programming language availible as part of PLT Scheme. It's completely separate to the main fluxus
;; commands, and represents a different way of creating games or other behavoural systems.
;; Example:
;; EndSectionDoc 

(module frisbee (lib "frtime-lang-only.ss" "frtime")
     
  (require 
   (lib "kw.ss")
   (lib "class.ss")
   (lib "match.ss")
   (lib "list.ss" "frtime")
   (lib "etc.ss" "frtime")
   (lib "math.ss" "frtime")
   (lib "lang-ext.ss" "frtime")
   (all-except (lib "frp-core.ss" "frtime") undefined?)
   ; import the procedures we want lifted
   (lifted (lib "fluxus.ss" "fluxus-0.15") vadd vsub vmul vdiv vtransform vtransform-rot
           vnormalise vdot vreflect vdist vmag vcross madd msub mdiv mmul mident mtranslate
           mrotate mscale mtranspose minverse maim qaxisangle qmul qnormalise qtomatrix qconjugate
           set-camera)
   (prefix flx- (lib "fluxus.ss" "fluxus-0.15")))
  
  (provide 
   (all-defined)
   (all-from (lib "frtime-lang-only.ss" "frtime")))
  
  ; make lifted versions of the vector procedures
  
  ;; StartFunctionDoc-en
  ;; vec3 x y z
  ;; Returns: result-vector
  ;; Description:
  ;; Creates a new vector usable inside frisbee - use this rather than (vector)
  ;; Example:
  ;; (vec3 1 2 3)
  ;; EndFunctionDoc
  
  (define (vec3 x y z)
    (lift #f vector x y z))
  
  ;; StartFunctionDoc-en
  ;; vec3-x v
  ;; Returns: result-number
  ;; Description:
  ;; Returns the x component of the frisbee vector
  ;; Example:
  ;; (vec3-x (vec3 1 2 3))
  ;; EndFunctionDoc
  
  (define (vec3-x v)
    (lift #t vector-ref v 0)) 
  
  ;; StartFunctionDoc-en
  ;; vec3-y v
  ;; Returns: result-number
  ;; Description:
  ;; Returns the y component of the frisbee vector
  ;; Example:
  ;; (vec3-y (vec3 1 2 3))
  ;; EndFunctionDoc
  
  (define (vec3-y v)
    (lift #t vector-ref v 1)) 
  
  ;; StartFunctionDoc-en
  ;; vec3-z v
  ;; Returns: result-number
  ;; Description:
  ;; Returns the z component of the frisbee vector
  ;; Example:
  ;; (vec3-z (vec3 1 2 3))
  ;; EndFunctionDoc
  
  (define (vec3-z v)
    (lift #t vector-ref v 2)) 
  
  ;; StartFunctionDoc-en
  ;; vec3-integral v
  ;; Returns: result-vector
  ;; Description:
  ;; Returns the integral of the frisbee vector in respect to time
  ;; Example:
  ;; (vec3-integral (vec3 0 0.01 0))
  ;; EndFunctionDoc
  
  (define (vec3-integral v)
    (vec3 (integral (vec3-x v))
          (integral (vec3-y v))
          (integral (vec3-z v))))
  
  ; some utils
  (define (flxrnd)
    (/ (random 10000) 10000))
  
  (define (rndvec)
    (vec3 (flxrnd) (flxrnd) (flxrnd)))
    
  ; deal with loading model data
  (define model-map '())
  
  (define (get-model filename)
    (let ((ret (assq filename model-map)))
      (cond ((list? ret) (cadr ret)) ; we have it already
            (else ; load and add it to the list
             (let ((prim (flx-obj-import filename)))
               (flx-with-primitive prim (flx-hide 1))
               (set! model-map (cons (list filename prim) model-map))
               ;(printf "~a~n" model-map)
               prim)))))  
  
  (define mouse (vector 0 0))
  (define mouse-left (event-receiver))
  (define mouse-middle (event-receiver))
  (define mouse-right (event-receiver))
  (define keyboard (event-receiver))
  (define fluxus-pulse (event-receiver)) 
  
  ;; standard behaviours
  
  (define clock 
    (hold 
     (map-e 
      (lambda (e) 
        (current-milliseconds)) 
      fluxus-pulse)))
  
  (define mouse-x
    (hold 
     (map-e 
      (lambda (e) 
        (vector-ref mouse 0)) 
      fluxus-pulse)))
  
  (define mouse-y
    (hold 
     (map-e 
      (lambda (e) 
        (vector-ref mouse 1)) 
      fluxus-pulse)))
  
  ; reusable frtime constructs and utils
  
  ;; StartFunctionDoc-en
  ;; scene collide-b
  ;; Returns: void
  ;; Description:
  ;; Sets the frisbee scene up. The list can contain primitive structures, or more lists.
  ;; Example:
  ;; (scene (list (cube)))
  ;; EndFunctionDoc
  
  (define (collide-b proc init pos-a pos-b radius )
    (hold
     (map-e
      (lambda (_)
        (snapshot/apply proc pos-a pos-b))
      (when-e (< (vdist pos-a pos-b) 2)))
     init))
  
  (define (key-control-b inc-key dec-key step)
    (integral 
     (hold 
      (map-e 
       (lambda (key) 
         (if (equal? key dec-key) (- step)
             (if (equal? key inc-key) step)))
       keyboard) 0)))
  
  (define (key-press-b key on off)
    (hold 
     (map-e
      (lambda (e)
        (if (eq? e key) on off))
      keyboard) 0))
  
  (define (key-time-e key)
    (map-e 
     (lambda (_)
       (value-now clock))
     (filter-e 
      (lambda (k) 
        (eq? k key)) 
      keyboard)))
  
  (define (metro tick)
    (let ((tick (floor (* tick 1000))))
      (when-e (> (modulo (floor milliseconds) tick) (/ tick 2)))))
  
  (define (truncate-list lst count)
    (cond 
      ((zero? count) '())
      ((null? lst) '())
      (else (cons (car lst) (truncate-list (cdr lst) (- count 1))))))
  
  (define (collision-with-list? pos with radius)
    (foldl
     (lambda (ob collided)
       (if (object-struct? ob)
           (if (< (vdist (object-struct-translate ob) pos) radius) 
               #t                  
               collided)
           collided))
     #f
     with))
  
  (define (factory proc event max-size)
    (collect-b 
     event '() 
     (lambda (e lst)
       (cons (proc e) (truncate-list lst max-size)))))
  
  (define (make-vector-grid w h d)
    (let zloop ((z d) (l '()))
      (cond ((zero? z) l)
            (else
             (zloop (- z 1)
                    (let yloop ((y h) (l l))
                      (cond ((zero? y) l)
                            (else 
                             (yloop (- y 1)
                                    (let xloop ((x w) (l l))
                                      (cond ((zero? x) l)
                                            (else
                                             (xloop (- x 1) (cons (vec3 x y z) l)))))))))))))) 
  
  ;---------------------------------------------------------------
  ; scene rendering
  
  ; the scene list contains everything we need to render
  (define scene-list '())
  
  ;; StartFunctionDoc-en
  ;; scene scene-list
  ;; Returns: void
  ;; Description:
  ;; Sets the frisbee scene up. The list can contain primitive structures, or more lists.
  ;; Example:
  ;; (scene (list (cube)))
  ;; EndFunctionDoc
  
  (define (scene s) 
    (set! scene-list s))

  ; the primitive object
  (define-struct object-struct (shape colour translate scale rotate matrix hints camera-lock texture))
  
  
  (define/kw (object #:key 
                     (shape 'cube)
                     (colour (vector 1 1 1))
                     (translate (vector 0 0 0))
                     (scale (vector 1 1 1))
                     (rotate (vector 0 0 0))
                     (matrix (flx-mident))
                     (hints '())
                     (camera-lock #f)
                     (texture ""))
    (make-object-struct shape colour translate scale rotate matrix hints camera-lock texture))

  
  (define (vector-now v)
    (let ((v (value-now v)))
      (if (void? v) 
          (vector 0 0 0)
          (vector (value-now (vector-ref v 0)) 
                  (value-now (vector-ref v 1))
                  (value-now (vector-ref v 2))))))
  
  (define (matrix-now v)
    (let ((v (value-now v)))
      (if (void? v) 
          (flx-mident)
          v)))
  
  
  ; render the scene list          
  (define (render-scene-list scene-list)
    
    (define (set-state colour translate scale rotate matrix hints texture)
      (flx-colour (vector-now colour))
      (flx-translate (vector-now translate))
      (flx-rotate (vector-now rotate))                             
      (flx-concat (matrix-now matrix)) 
      (flx-scale (vector-now scale))    
      (if (not (string=? texture ""))
          (flx-texture (flx-load-texture texture)))
      (for-each
       (lambda (hint)
         (case hint
           ((solid) (flx-hint-solid))
           ((wire) (flx-hint-wire))
           ((unlit) (flx-hint-unlit))
           (else (printf "error, unknown hint :~a ~n" hint))))
       (value-now hints)))
    
    (for-each
     (lambda (v)       
       (match (value-now v)
         [($ object-struct shape colour translate scale rotate matrix hints camera-lock texture)
          (flx-with-state
           (set-state colour translate scale rotate matrix hints texture)
           (if camera-lock (flx-set-camera 
                            (flx-mmul (flx-get-transform)
                                      (flx-mmul
                                       (flx-mtranslate (vec3 0 0 -10))
                                       (flx-mrotate (vec3 90 0 0))))))
           (cond 
		   	 ((string? shape) (flx-draw-instance (get-model shape)))
		     (else
			   (case shape
                 ((cube) (flx-draw-cube))
                 ((sphere) (flx-draw-sphere))
                 ((torus) (flx-draw-torus))
                 ((plane) (flx-draw-plane))
                 (else (printf "render-scene-list: unknown object shape: ~a~n" shape))))))]
         [(? undefined?) (void)]
         [(? list?)          
          (render-scene-list (value-now v))]
         [(? void?) (void)]))
     (value-now scene-list)))
  
  ; convert inputs to events
  (define (read-inputs)
  	(if (flx-mouse-button 1) (send-event mouse-left #t))
  	(if (flx-mouse-button 2) (send-event mouse-middle #t))
  	(if (flx-mouse-button 3) (send-event mouse-right #t))
	(set! mouse (vector (flx-mouse-x) (flx-mouse-y)))
	(send-event keyboard #f)
	(for-each
		(lambda (key)
			(send-event keyboard key))
		(flx-keys-down)))

  ; syncronising rendering with frtime
  (define (loop)
    ;; emit the next pulse
	(read-inputs)
    (send-event fluxus-pulse #t)
    ;; wait for frtime to update
    (do-in-manager-after (void))
	(render-scene-list scene-list))
  
  (define (init-me)
    (flx-every-frame (loop)))
  
  (init-me)
  
  )

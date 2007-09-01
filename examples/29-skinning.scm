;; skinning test script

(clear)
; setup lights
(light-diffuse 0 (vector 0.4 0.4 0.4))
(define l (make-light 'point 'free))
(light-diffuse l (vector 1 1 1))
(light-position l (vector 50 50 0))
(shadow-light l)
(shadow-debug 0)

; build a simple skeleton
(push)
(hint-origin)
; build the bindpose at the same time 
; as the animated pose
(define b1 (build-locator))
(define s1 (build-locator))
(pop)

(push)
(hint-origin)
(translate (vector 0 2 0))
(parent b1)
(define b2 (build-locator))
(parent s1)
(define s2 (build-locator))
(pop)

(push)
(hint-origin)
(translate (vector 0 2 0))
(parent b2)
(define b3 (build-locator))
(parent s2)
(define s3 (build-locator))
(pop)

; make our pfuncs
(define gs (make-pfunc 'genskinweights))
(define sc (make-pfunc 'skinweights->vertcols))
(define s (make-pfunc 'skinning))

; set up the parameters
(pfunc-set! gs (list 'skeleton-root b1
                    'sharpness 4.0))

(pfunc-set! s (list 'bindpose-root s1 
                    'skeleton-root b1
                    'skin-normals 0))

; make the primitive we are going to skin
(push) 
(hint-vertcols)
(hint-cast-shadow)
(scale (vector 0.5 4 0.5))
(define o (build-cylinder 20 20))
(pop)
(apply-transform o)

; set up the pdata 
(grab o)
; generate weights
(pfunc-run gs)
; view them as vertcols
(pfunc-run sc)
(pdata-copy "p" "pref")
(pdata-copy "n" "nref")
(ungrab)

(push)
(colour (vector 0.2 0.4 0.4))
(rotate (vector 90 0 0))
(scale (vector 50 50 50))
(build-plane)
(pop)

(define (animate)
    (grab b2)
    (identity)
    (translate (vector 0 2 0))
    (rotate (vector (* 130 (sin (time))) 0 0))
    (ungrab)
    (grab o)
    (pfunc-run s)
    (recalc-normals 1)
    (ungrab))

(every-frame (animate))

(desiredfps 1000)
(show-fps 1)

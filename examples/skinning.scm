;; skinning test script

(clear)
; setup lights
(light-diffuse 0 (vector 0.4 0.4 0.4))
(define l (make-light 'point 'free))
(light-diffuse l (vector 1 1 1))
(light-position l (vector 50 50 0))
(shadow-light l)
(shadow-debug 0)

; build a simple skeleton - this is a bit convoluted at
; present, but the idea is that these will eventually be
; loaded or procedurally generated
(define b1 (with-state
    (hint-origin)
    (build-locator)))

(define s1 (with-state
    (hint-origin)
    (build-copy b1)))

(define b2 (with-state
    (hint-origin)
    (translate (vector 0 2 0))
    (parent b1)
    (build-locator)))

(define s2 (with-state
    (hint-origin)
    (translate (vector 0 2 0))
    (parent s1)
    (build-locator)))

(define b3 (with-state
    (hint-origin)
    (translate (vector 0 2 0))
    (parent b2)
    (build-locator)))

(define s3 (with-state
    (hint-origin)
    (translate (vector 0 2 0))
    (parent s2)
    (build-locator)))

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
(define o (with-state
    (hint-vertcols)
    (hint-cast-shadow)
    (scale (vector 0.5 4 0.5))
    (build-cylinder 20 20)))

(with-primitive o
    (apply-transform)
    ; set up the pdata 
    ; generate weights
    (pfunc-run gs)
    ; view them as vertcols
    (pfunc-run sc)
    (pdata-copy "p" "pref")
    (pdata-copy "n" "nref"))

(with-state
    (colour (vector 0.2 0.4 0.4))
    (rotate (vector 90 0 0))
    (scale (vector 50 50 50))
    (build-plane))

(define (animate)
    (with-primitive b2
        (identity)
        (translate (vector 0 2 0))
        (rotate (vector (* 130 (sin (time))) 0 0)))
    (with-primitive o
        (pfunc-run s)
        (recalc-normals 1)))

(every-frame (animate))

(desiredfps 1000)
(show-fps 1)

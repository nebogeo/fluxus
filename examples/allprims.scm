; renders all the primitives that fluxus supports

(clear)
(clear-colour (vector 0.5 0.5 0.5))
(define tex (load-texture "test.png"))

; poly cube
;(clear-colour (vector 1 1 1))
(texture tex)
(build-cube)

; poly sphere
(translate (vector 1.5 0 0))
(with-state
    (scale (vector 0.5 0.5 0.5))
    (build-sphere 30 30))

; poly cylinder
(translate (vector 1.5 0 0))
(with-state
    (translate (vector 0 -0.5 0))
    (scale (vector 0.5 1 0.5))
    (build-cylinder 30 30))

; poly plane
(translate (vector 1.5 0 0))
(build-plane)

; poly lines
(translate (vector 1.5 0 0))
(with-state
    (hint-unlit)
    (with-primitive (build-ribbon 4)
        (pdata-set "p" 0 (vector 0 0 0))
        (pdata-set "p" 1 (vector 1 0 0))
        (pdata-set "p" 2 (vector 0.5 1.3 0))
        (pdata-set "p" 3 (vector 1.2 1 0))
        (pdata-set "w" 0 0.1)
        (pdata-set "w" 1 0.1)
        (pdata-set "w" 2 0.01)
        (pdata-set "w" 3 0.1)))

; nurbs sphere
(translate (vector -6 2 0))
(texture tex)

(with-state
    (hint-points)
    (point-width 3)
    (scale (vector 0.5 0.5 0.5))
    (with-primitive (build-nurbs-sphere 10 20)
        ; tweak a vertex to prove it's curvy
        (pdata-set "p" 95 (vector -1 1 1))
        (recalc-normals 1)))

; nurbs plane
(translate (vector 1.5 0 0))
(with-state
    (scale (vector 2 2 2))
    (hint-points)
    (point-width 3)
    (scale (vector 0.5 0.5 0.5))
    (with-primitive (build-nurbs-plane 10 10)
        ; tweak a vertex to prove it's curvy
        (pdata-set "p" 40 (vector 0.4 0.4 1))
        (recalc-normals 1)))

; particles
(translate (vector 1.0 -.5 0))
(with-state
    (hint-none)
    (hint-points)
    (point-width 3)
    (with-primitive (build-particles 100)
        ; randomise the particle positions
        (pdata-map!
            (lambda (p)
                (rndvec))
            "p")))

; sprite particles
(translate (vector 1.5 0 0))
(with-state
    (point-width 10)
    ; a function to init the particle points and colours
    (hint-anti-alias)
    (with-primitive (build-particles 100)
        ; randomise the particle positions
        (pdata-map!
            (lambda (p)
                (vector (flxrnd) (flxrnd) (flxrnd)))
            "p")

        ; set the particle colours to white
        (pdata-map!
            (lambda (c)
                (vector 1 1 1))
            "c")))
    

; blobby primitive
(translate (vector 1.5 0 0))
(with-state
    (point-width 10)
    (with-primitive (build-blobby 3 (vector 20 20 20) (vector 1 1 1))
        ; randomise the influence positions
        (pdata-map!
            (lambda (p)
                (vector (flxrnd) (flxrnd) (flxrnd)))
            "p")
        ; set the radius of the influences
        (pdata-map! (lambda (s) 0.05) "s")))


; pixels primitive
(translate (vector -6 1.5 0))
(with-primitive (build-pixels 100 100)
    ; randomise the influence positions
    (pdata-map!
        (lambda (c)
            (vector 0 0 (flxrnd) 1))
        "c")
    (pixels-upload))

; locator primitive (ohh - interesting!)
(translate (vector 1.5 0 0))
(with-state
    (hint-origin)
    (build-locator))

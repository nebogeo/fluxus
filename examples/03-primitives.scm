; exhaustively render all the primitives that fluxus supports

(clear)

(define tex (load-texture "textures/test.png"))

; poly cube
(clear-colour (vector 1 1 1))
(texture tex)
(build-cube)

; poly sphere
(translate (vector 1.5 0 0))
(push)
(scale (vector 0.5 0.5 0.5))
(build-sphere 30 30)
(pop)

; poly cylinder
(translate (vector 1.5 0 0))
(push)
(translate (vector 0 -0.5 0))
(scale (vector 0.5 1 0.5))
(build-cylinder 30 30)
(pop)

; poly plane
(translate (vector 1.5 0 0))
(build-plane)

; poly lines
(translate (vector 1.5 0 0))
(push)
(hint-unlit)
(build-line (vector 0 0 0) 0.1 (vector 1 0 0) 0.05)
(build-line (vector 1 0 0) 0.05 (vector 0.5 1.3 0) 0.01)
(build-line (vector 0.5 1.3 0) 0.01 (vector 1.2 1 0) 0.1)
(pop)

; poly text
(translate (vector 2 0 0))
(push)
(colour (vector 0 0 0))
(hint-unlit)
(scale (vector 0.2 0.2 0.2))
(texture (load-texture "textures/scribblefont.png"))
(build-text "hello")
(pop)

; nurbs sphere
(translate (vector -8 2 0))
(texture tex)
(push)
(hint-points)
(point-width 3)
(scale (vector 0.5 0.5 0.5))
(let ((ob (build-nurbs-sphere 10 20)))
; tweak a vertex to prove it's curvy
(pdata-set "p" 1 (vector 1 0 0)))
(pop)

; nurbs plane
(translate (vector 1 0 0))
(push)
(translate (vector 0 0.5 0))
(scale (vector 2 2 2))
(rotate (vector 90 0 0))
(hint-points)
(point-width 3)
(scale (vector 0.5 0.5 0.5))
(let ((ob (build-nurbs-plane 10 10)))
; tweak a vertex to prove it's curvy
(pdata-set "p" 1 (vector 0 1 0)))
(pop)

; particles
(translate (vector 1.5 -0.5 0))
(push)
(point-width 10)
; a function to init the particle points and colours
(define (particle-init n)
    (pdata-set "p" n (vector (flxrnd) (flxrnd) (flxrnd)))
    (pdata-set "c" n (vector (flxrnd) (flxrnd) (flxrnd)))
    (if (< n 1)
        0
        (particle-init (- n 1))))
(hint-anti-alias)
(define particles (build-particles 100))
(pop)
(grab particles)
(particle-init (pdata-size))
(ungrab)

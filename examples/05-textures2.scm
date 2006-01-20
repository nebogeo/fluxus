; a script adapted from a live one I use
; just a simple textured plane thing that animates to sound
; it's better when you use textures that acually look good of course...

; constants
(define tr 8)
(define sc 1)
(define zpos 0)
(define t 0)
(define texture-location "textures/")

; a function to put filenames of all the png images in a directory into a list
; this is a useful function, as new textures added to the texture location will
; be automatically picked up by the script without any modifications
(define get-textures
	(lambda (d l)
		(let ((ret (readdir d)))
			(cond ((not (string? ret))
				(closedir d)
				l)
				((if (and (> (string-length ret) 4)
						(string=? (substring ret (- (string-length ret) 4)) ".png"))
					(set! l (append l (list ret))))
				(get-textures d l))))))

; a list of all the textures
(define texnames (get-textures (opendir texture-location) '()))

; a list to hold the objects
(define elements '())

; make a new textured plane 
(define (add-element tex)
    (push)
    (opacity 1) 
    (texture (load-texture (string-append texture-location tex)))
    (translate (vector (* tr (flxrnd)) (* tr (flxrnd)) zpos))
    (set! zpos (+ 0.01 zpos))
    (set! t (* sc (flxrnd)))
    (scale (vector t t 1))
    (set! elements (cons (build-plane) elements))
    (pop))

; build a plane for every texture in the list, and add it to
; the elements list
(define (build-all l)
    (add-element (car l))
    (if (eq? '() (cdr l))
        '()
        (build-all (cdr l))))

; call build-all n times
(define (build n)
    (build-all texnames)
    (if (eq? n 0)
        0
        (build (- n 1))))

; rotate the objects in the list according to the audio
(define (animate l c)
    (grab (car l))
    (rotate (vector 0 0 (* 10 (- (gh c) 0.5))))
    (ungrab)
    (if (eq? (cdr l) '())
        '()
        (animate (cdr l) (+ 1 c))))

(define (render)
    (animate elements 1))

(clear)
(build  20)
(every-frame "(render)")

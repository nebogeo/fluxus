; an example of collisions, the ground plane and joints

(clear)
(collisions 1)

; links all the objects in the list together
(define (link l c)
    (cond ((not (null? (cdr l)))
        (build-balljoint (car l) (cadr l) 
            (vector 0 (* 1.1 (- c 0.5)) 0))
        (link (cdr l) (- c 1)))))

; builds the objects          
(define (array n l)
    (cond 
        ((zero? n) l)
        (else
            (array (- n 1) 
                (cons (with-state
                    (translate (vector 0 (* n 1.1) 0))
                    (scale (vector 0.7 1 0.2))
                    (let ((obj (build-cube)))
                        (active-box obj)
                        obj)) 
                l)))))
            
; set gravity up (I think this is default anyway, but try changing it)
(gravity (vector 0 -1 0))
; setup a groundplane to collide with
(ground-plane (vector 0 1 0) 0)

; make and join the boxes togther
; the reverse is just to make the list the correct
; order for linking
(link (reverse (array 20 '())) 20)

; show the joints as locators (really useful for debugging)
(set-physics-debug #t)
(clear)

; random direction for spraycan like effect
(define (spread dir amount)
    (vadd (vmul (grndvec) amount) dir))

(define (spray-init pos dir speed col)
    (pdata-add "vel" "v")
    (pdata-map!
        (lambda (p)
            pos)
        "p")
    (pdata-map!
        (lambda (c)
            col)
        "c")
    (pdata-map!
        (lambda (vel) 
            (vmul (spread dir 0.1) speed))
        "vel"))

(define (spray-update pos dir speed)

    (define (collide-paint? pos col)
        (with-primitive s
            (let ((i (line-intersect pos (vadd pos (vmul dir 0.1)))))
                (cond 
                    ((not (null? i))
    
                        (let* ((tc (cdr (assoc "t" (car i))))
                               (tu (inexact->exact (round (* (vector-ref tc 0) 100))))
                               (tv (inexact->exact (round (* (vector-ref tc 1) 100)))))
                        
                        (with-primitive t
                            (pdata-set! "c" (+ tu (* tv 100)) col)
                            (pixels-upload)))
                                            
                        #t)
                    (else
                        #f)))))

    (pdata-op "+" "p" "vel")
    (let ((reset (random (pdata-size))))
        (pdata-set! "p" reset pos)
        (pdata-set! "vel" reset (vmul (spread dir 0.1) speed)))
    (pdata-map!
        (lambda (p c)
            (if (collide-paint? p c)
                pos
                p))
            "p" "c"))


;----------------------------------------
; build the primitives

(define t (build-pixels 100 100))
(with-primitive t 
    (translate (vector 3 0 0))
    (pdata-map!
        (lambda (c)
            (vector 1 1 1 1))
        "c")
    (pixels-upload)
;    (hide 1)
    )

(define s (with-state
    (texture (pixels->texture t))
    (build-torus 0.5 1 10 10)))

(define p (with-state
    (hint-none)
    (hint-points)
    (point-width 6)
    (hint-anti-alias)
    (build-particles 100)))

(define p2 (with-state
    (hint-none)
    (hint-points)
    (point-width 6)
    (hint-anti-alias)
    (build-particles 100)))

;---------------------------------
; init and run the thing

(define spray-pos (vector 0 2.5 3))
(define spray-dir (vector 0 -0.5 -1))
(define spray-speed 0.03)
(define spray-col (vector 1 0 0))

(define spray2-pos (vector 2.4 1.5 1))
(define spray2-dir (vector -0.7 -0.5 -0.2))
(define spray2-speed 0.03)
(define spray2-col (vector 0 1 0))

(with-primitive p 
    (spray-init spray-pos spray-dir spray-speed spray-col))

(with-primitive p2
    (spray-init spray2-pos spray2-dir spray2-speed spray2-col))

(blur 0.1)

(every-frame
    (begin
        (with-primitive p
            (spray-update spray-pos spray-dir spray-speed))
        (with-primitive p2
            (spray-update spray2-pos spray2-dir spray2-speed))))

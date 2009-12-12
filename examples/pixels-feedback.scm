;--- Simple example of how to do texture feedback
;--- By Evan Raskob http://pixelist.info



(clear)




(hint-unlit)

;-- create 2 renderer buffers
(define renderers 
    (list
        (build-pixels 1024 1024 #t)
        (build-pixels 1024 1024 #t)
        )
    )

;-- hide renderers
(map (lambda (n) (with-primitive n 
            (with-state
                (scale 20)
                (colour #( 1 1))
                (draw-plane)
                )
            (scale 0))) renderers)


;-- note - texture must be set inside pixels-render
;-- but primitive id DOESN'T exist inside it

(clear-colour #(0 0 0))

(define (draw)
    (let* [(r1 (car renderers))
            (r2 (car (cdr renderers)))
            (t1 (pixels->texture r1))
            (t2 (pixels->texture r2))
]
        
        
        (with-pixels-renderer r1
            (clear-colour #(1 1))
            ;(hint-unlit)
            (rotate (vector 0 (* 10 (delta)) 0))
            (with-state                
                (texture t2)
                (scale 6)
                (colour #( 1 0 1))
                (draw-cube)
                )
            )
        

        (with-pixels-renderer r2
            (clear-colour #(1 1))
            ;(hint-unlit)
            (rotate (vector 0 (* (delta) 10) 0))
            (with-state                
                (texture t1)
                (scale 6)
                (colour #( 1 0 1))
                (draw-cube)
                )
            )
        
        (with-state
            (scale 20)
            (texture (pixels->texture r1))
            (draw-plane))
        
        (set! renderers (list r2 r1))
        )
    )


(every-frame (draw))

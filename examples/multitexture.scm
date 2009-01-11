(clear)
(define (make-torus a b ea eb)
    (with-state
        (scale 0.02)
        (translate (vector -20 -25 10))
        (hint-unlit)
        (build-type "Bitstream-Vera-Sans-Mono.ttf" 
            (string-append (symbol->string ea) "/" (symbol->string eb))))

    (let ((p (build-torus 1 2 20 20)))
        
        (with-primitive p
            (scale 0.15)
            (multitexture 0 (load-texture a))
            (texture-params 0 (list 'tex-env ea))
            (multitexture 1 (load-texture b))
            (texture-params 1 (list 'tex-env eb))
            (pdata-copy "t" "t1")
            (pdata-map!
                (lambda (t1)
                    (vmul t1 5))
                "t1"))))

(for-each
    (lambda (ea)
        (with-state
        (for-each            
            (lambda (eb)
                (make-torus "refmap.png" "transp.png" ea eb)
                (translate (vector 1 0 0)))
            '(modulate decal blend replace)))
        (translate (vector 0 1 0)))
    '(modulate decal blend replace))


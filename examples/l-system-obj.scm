;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; lsystem example with obj import/export to provide a sample
; workflow using blender (for instance)
; warning - this script can result in lots of geometry to save!

; try all the rules on this character - 
; returns #f if none are matched
(define (lsys-run-rules char rules)
  (foldl
   (lambda (rule str)
     (if str ; if str is not #f
         str ; then we have already found a rule, return it
         (if (char=? char (string-ref (car rule) 0)) ; check this rule
             (cadr rule) ; return the string
             #f))) ; no match
   #f
   rules))

; runs the lsystem rules on every character in a string, 
; returns the new string
(define (lsys-search-replace str rules pos result)
  (cond 
    ((>= pos (string-length str)) result)
    (else
     (let ((ret (lsys-run-rules (string-ref str pos) rules)))
       (if (string? ret)
           (lsys-search-replace str rules (+ pos 1) 
                                (string-append result ret))
           (lsys-search-replace str rules (+ pos 1) 
                                (string-append result (string (string-ref str pos)))))))))

; runs the search-replace multiple (n) times on a string
(define (ls-generate n str rules)
  (cond 
    ((zero? n) str)
    (else
     (ls-generate (- n 1) 
                  (lsys-search-replace str rules 0 "") rules))))

; builds objects from a string, and builds up a 
; list of object ids 
(define (ls-build string angle branch-scale model)
  (foldl 
   (lambda (char obj-list)
     (case char 
       ((#\F)
        (translate (vector 1 0 0))
        (with-state 
         (scale (vector 1 0.5 1))
         (cons (build-copy model) obj-list)))
       ((#\f) (translate (vector 1 0 0)) obj-list)
       ((#\/) (rotate (vector angle 0 0)) obj-list)
       ((#\\) (rotate (vector (- angle) 0 0)) obj-list)
       ((#\+) (rotate (vector 0 angle 0)) obj-list)
       ((#\-) (rotate (vector 0 (- angle) 0)) obj-list)
       ((#\^) (rotate (vector 0 0 angle)) obj-list)
       ((#\&) (rotate (vector 0 0 (- angle))) obj-list)
       ((#\|) (rotate (vector 0 0 180)) obj-list)
       ((#\[) (push) (scale (vector branch-scale branch-scale branch-scale))
              obj-list)
       ((#\]) (pop) obj-list)))
   '()
   (string->list string)))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(clear)

; load an obj model to duplicate (needs to be triangulated, 
; and keep the poly count down as much as possible)
(define model (obj-make (obj-load "widget.obj")))

; hide it, as we just want to use it for instancing copies according to the l-system rules
(with-primitive model
                (hide 1))

; build the thing (using the l-system rules and model)
(define obj-list (ls-build (ls-generate 5 "F" '(("F" "F[^F][&F]"))) 86 0.7 model))
  
; uncomment to save out all the objects to an obj file
;(obj-export "ls-out.obj" obj-list 'triangle-list)



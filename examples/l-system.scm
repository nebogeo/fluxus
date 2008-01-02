; Lindenmayer System growth
; -------------------------
;
; Repeatedly applies production rules to strings, and
; interprets the strings to form graphics, according to
; the method described in "The Algorithmic Beauty of 
; Plants" by Przemyslaw Prusinkiewicz and Aristid 
; Lindenmayer

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

; builds objects from a string
(define (ls-build string angle branch-scale)
  (for-each 
   (lambda (char)
     (cond 
       ((char=? #\F char)
        (with-state
         (translate (vector 0.5 0 0))
         (scale (vector 1 0.1 0.1))
         (build-cube))
        (translate (vector 1 0 0)))
       ((char=? #\f char)
        (translate (vector 1 0 0)))
       ((char=? #\/ char)
        (rotate (vector angle 0 0)))
       ((char=? #\\ char)
        (rotate (vector (- angle) 0 0)))
       ((char=? #\+ char)
        (rotate (vector 0 angle 0)))
       ((char=? #\- char)
        (rotate (vector 0 (- angle) 0)))
       ((char=? #\^ char)
        (rotate (vector 0 0 angle)))
       ((char=? #\& char)
        (rotate (vector 0 0 (- angle))))
       ((char=? #\| char)
        (rotate (vector 0 0 180)))
       ((char=? #\[ char)
        (push)
        (scale (vector branch-scale branch-scale branch-scale)))
       ((char=? #\] char)
        (pop))))
   (string->list string)))

(clear)
; run the actual lsystem code
(ls-build (ls-generate 7 "F" '(("F" "F[^F][&F]"))) 86 0.7))
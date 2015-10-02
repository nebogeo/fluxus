;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

#lang racket/base
(require scheme/class)
(require "fluxus.ss")

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; ring menu (originally from betablocker)
; ring menus are great with analogue controllers as they allow fast selection
; from quite a large range - and the positions can be memorised quickly

(provide ringmenu%)

; items is a list of strings for the menu selector
(define ringmenu%
  (class object%
    (init-field 
     (attach #f)
     (position (vector 0 0))
     (tx 0)
     (items '()))
    
    (field
     (objs '())
     (selected 0)
     (shown #f)
     (root 0)
     (size 1)
     (item-size 1)
     (deadzone 0.1)
     (menu-colour (vector 0.5 0.5 0.5))
     (menu-hi-colour (vector 1 1 1)))
    
    (define/public (get-root)
      root)
    
    (define/public (get-selected)
      selected)
    
    (define/public (get-shown)
      shown)
    
    (define/public (get-position)
      position)
  
    (define (list-set l n v)
      (cond
        ((null? l) '())
        ((zero? n) (cons v (cdr l)))
        (else (cons (car l) (list-set (cdr l) (- n 1) v)))))
    
    (define/public (update-item n s)
      (set! items (list-set items n s))
      (grab (list-ref objs n))
      (pdata-set! "t" 4 s)
      (pdata-set! "t" 7 (vadd s (vector 0.25 0 0)))
      (pdata-set! "t" 6 (vadd s (vector 0.25 0.25 0)))
      (pdata-set! "t" 5 (vadd s (vector 0 0.25 0)))
      (ungrab))
    
    (define/public (set-position pos)
      (set! position pos)
      (cond 
        ((not (null? objs))
         (grab root)
         (identity)
         (translate pos)
         (scale (vector size size size))
         (ungrab))))
    
	(define/public (set-item-size s)
		(set! item-size s))
	
    (define/public (build)
      (define (loop n len tc)
        (push)
        (let ((angle (+ 180 (* 360 (/ n len)))))
          (rotate (vector 0 0 angle))
          (translate (vector 1.5 0 0))
          (rotate (vector 0 0 (- angle))))
        (cond ((zero? n) 
              (translate (vector 0 0 0.1)) ; compensate for the selection
              (scale (vector item-size item-size 1)))
			(else
			  (scale (vector (* item-size 0.5) (* item-size 0.5) 1)))) ; don't scale the selected one
        (set! objs (cons (build-plane) objs))
        (pop)
        
        (grab (car objs))
        ; just set the forward facing quad texture coords
        (pdata-set! "t" 4 (car tc))
        (pdata-set! "t" 7 (vadd (car tc) (vector 0.25 0 0)))
        (pdata-set! "t" 6 (vadd (car tc) (vector 0.25 0.25 0)))
        (pdata-set! "t" 5 (vadd (car tc) (vector 0 0.25 0)))
        (ungrab)
        
        (if (zero? n)
            0
            (loop (- n 1) len (cdr tc))))
      
      (let ((len (length items)))
        (push)
        (scale (vector 0.3 0.3 0.3))
        (hint-unlit) 
        (colour menu-colour)
        (translate (vector 0 0 -1))
        ;(hint-ignore-depth)
        (hint-depth-sort)
        (parent attach)
        (rotate (vector 0 0 0))
        (set! root (build-locator))
        (parent root)
        (texture tx)
        (loop (- len 1) len (reverse items)))
      (pop)
      (hide-menu))
    
    (define/public (update joylisten axis)
      (define (ang y x)
        (let ((q (/ 3.141 2)))
          (when (zero? y) (set! y 0.0001))
          (cond 
            ((>= y 0)
             (+ q q (- (atan (/ x y)))))
            (else
             (- q q (atan (/ x y)))))))
      
      (let ((s (sqrt (+ (* (vector-ref (send joylisten get-axis axis) 0) 
                           (vector-ref (send joylisten get-axis axis) 0))
                        (* (vector-ref (send joylisten get-axis axis) 1) 
                           (vector-ref (send joylisten get-axis axis) 1))))))
        (when (> s 0.5) (set! s 0.5))
        (set! size (* s 0.5)))
      
      
      ; deadzone
      (cond
        ((or (or (> (vector-ref (send joylisten get-axis axis) 0) deadzone)
                 (< (vector-ref (send joylisten get-axis axis) 0) (- deadzone)))
             (or (> (vector-ref (send joylisten get-axis axis) 1) deadzone)
                 (< (vector-ref (send joylisten get-axis axis) 1) (- deadzone))))
         
         (show-menu)
         (menu-select (inexact->exact 
                       (round (* (length items)
                                 (/ (ang (vector-ref (send joylisten get-axis axis) 0)
                                         (vector-ref (send joylisten get-axis axis) 1)) 
                                    (* 3.141 2)))))))
        (else
         (hide-menu))))
    
    (define/public (menu-select sel-count)
      (grab (list-ref objs selected))
      (translate (vector 0 0 -0.1))
      (colour menu-colour)    
      (scale (vector 0.5 0.5 0.5))
      (ungrab)
      (set! selected (modulo sel-count (length items)))
      (grab (list-ref objs selected))
      (scale (vector 2 2 2))
      (colour menu-hi-colour)    
      (translate (vector 0 0 0.1))
      (ungrab))
    
    (define/public (show-menu)
      (grab root)
      (hide 0)
      (ungrab)
      (set! shown #t))
    
    (define/public (hide-menu)
      (grab root)
      (hide 1)
      (ungrab)
      (set! shown #f))
    
    (super-new)))

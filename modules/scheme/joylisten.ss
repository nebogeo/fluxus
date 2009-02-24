;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

#lang scheme/base
(require scheme/class)
(require fluxus-016/fluxus)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; joystick input listener

(provide joylisten%)

(define joylisten%
  (class object%

    (field
     (buttons (make-buttons 16 '()))
     (axes (make-axes 16 '()))
     (button-state (make-buttons 16 '()))
     (hats-to-axes (list (cons 0 2))) ; hats to axes conversion list,
                                      ; default: hat 0 -> axis 2
     (device "0"))

    (define/public (set-device-num! s)
        (set! device s))

    (define/public (set-hats-to-axes! l)
        (set! hats-to-axes l))

    (define (list-set l n v)
      (cond
        ((null? l) '())
        ((zero? n) (cons v (cdr l)))
        (else (cons (car l) (list-set (cdr l) (- n 1) v)))))

    (define (hat->axis n)
      (let ((axis (assoc n hats-to-axes)))
        (if axis
            (cdr axis)
            n)))

    (define/public (make-axes n l)
      (if (zero? n)
          l
          (make-axes (- n 1) (cons (make-vector 2 0) l))))

    (define/public (make-buttons n l)
      (if (zero? n)
          l
          (make-buttons (- n 1) (cons 0 l))))

    (define/public (get-button n)
      (list-ref buttons n))

    (define (button-changed n)
      (list-ref button-state n))

    (define/public (get-axis n)
      (list-ref axes n))

    (define (set-button! n s)
      (set! buttons (list-set buttons n s)))

    (define/public (update)
      (define (drain path value)       ; drains all osc events for a message and 
        (if (osc-msg path)             ; only reports the last one which is ok for
            (drain path (osc 0))       ; this sort of control
            value))

      (define (do-axes n)
        (let ((value (drain (string-append "/oscjoy." device ".axis." (number->string n)) #f)))
          (cond
            ((number? value)
             ; do some mangling of values here,
             ; firstly, make 0=x 1=y and secondly,
             ; change the range from 0 to 1 to -1 to 1
             (vector-set! (get-axis (inexact->exact (truncate (/ n 2))))
                          (- 1 (modulo n 2)) (* 2 (- value 0.5))))))
        (if (zero? n)
            0
            (do-axes (- n 1))))

      ; convert .hat messages to .axis values - necessary for dpad messages on osx
      (define (do-hats n)
        (let ((valuex (drain (string-append "/oscjoy." device ".hat." (number->string n) ".x") #f))
              (valuey (drain (string-append "/oscjoy." device ".hat." (number->string n) ".y") #f)))
          (cond
            ((and (number? valuex) (number? valuey))
             (set! valuex (- 1 valuex))
             ; do some mangling of values here,
             ; firstly, make 0=x 1=y and secondly,
             ; change the range from 0 to 1 to -1 to 1
             ; convert dhat to axis
             (let ((axis (hat->axis (inexact->exact n))))
                 (vector-set! (get-axis axis) 1 (* 2 (- valuex 0.5)))
                 (vector-set! (get-axis axis) 0 (* 2 (- valuey 0.5)))))))
        (if (zero? n)
            0
            (do-hats (- n 1))))

      (define (do-buttons n)
        (let ((value (drain (string-append "/oscjoy." device ".button." (number->string n)) #f)))
          (cond 
            ((number? value)
             ; have we changed?
             (if (not (eq? (get-button n) value))
                 (set! button-state (list-set button-state n #t))
                 (set! button-state (list-set button-state n #f)))
             (set-button! n value))))
        (if (zero? n)
            0
            (do-buttons (- n 1))))

      ;(display (osc-peek)) (newline)

      (do-axes 16)
      (do-hats 16)
      (do-buttons 16))

    (super-new)))


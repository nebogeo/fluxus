;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

; some random helpers

;; StartSectionDoc-en
;; scheme-utils
;; High level fluxus commands written in Scheme.
;; Example:
;; EndSectionDoc

(module fluxus racket

(require "fluxus-modules.ss")
(require "building-blocks.ss")

(provide (all-defined-out))

;; StartFunctionDoc-en
;; rndf
;; Returns: number
;; Description:
;; Returns a random number in the range 0->1
;; Example:
;; (display (rndf))(newline)
;; EndFunctionDoc

(define (rndf)
  (* (random 10000) 0.0001))

;; StartFunctionDoc-en
;; crndf
;; Returns: number
;; Description:
;; Returns a random number in the range -1->1 (centred on zero)
;; Example:
;; (display (crndf))(newline)
;; EndFunctionDoc

(define (crndf)
  (* (- (rndf) 0.5) 2))

;; StartFunctionDoc-en
;; rndvec
;; Returns: vector
;; Description:
;; Returns a random 3 element vector with each element in the range 0->1. If
;; you visualise a lot of these as points, they will fill the unit cube
;; (see the example).
;; Example:
;; (clear)
;; (hint-none)
;; (hint-points)
;; (point-width 4)
;; (define p (build-particles 1000))
;;
;; (show-axis 1)
;;
;; (with-primitive p
;;     (pdata-map!
;;         (lambda (p)
;;             (vector 1 1 1))
;;         "c")
;;     (pdata-map!
;;         (lambda (p)
;;             (rndvec))
;;         "p"))
;; EndFunctionDoc

(define (rndvec)
  (vector (rndf) (rndf) (rndf)))

;; StartFunctionDoc-en
;; crndvec
;; Returns: vector
;; Description:
;; Returns a random 3 element vector with each element in the range -1->1. If
;; you visualise a lot of these as points, they will fill a cube centred
;; on the origin (see the example).
;; Example:
;; (clear)
;; (hint-none)
;; (hint-points)
;; (point-width 4)
;; (define p (build-particles 1000))
;;
;; (show-axis 1)
;;
;; (with-primitive p
;;     (pdata-map!
;;         (lambda (p)
;;             (vector 1 1 1))
;;         "c")
;;     (pdata-map!
;;         (lambda (p)
;;             (crndvec))
;;         "p"))
;; EndFunctionDoc

(define (crndvec)
  (vector (crndf) (crndf) (crndf)))

;; StartFunctionDoc-en
;; srndvec
;; Returns: vector
;; Description:
;; Returns a random 3 element vector. If you visualise a lot of these as
;; points, they will fill a sphere centred on the origin (see the example).
;; Example:
;; (clear)
;; (hint-none)
;; (hint-points)
;; (point-width 4)
;; (define p (build-particles 1000))
;;
;; (show-axis 1)
;;
;; (with-primitive p
;;     (pdata-map!
;;         (lambda (p)
;;             (vector 1 1 1))
;;         "c")
;;     (pdata-map!
;;         (lambda (p)
;;             (srndvec))
;;         "p"))
;; EndFunctionDoc

(define (srndvec)
  (let loop ((v (crndvec)))
    (if (> (vmag v) 1) ; todo: use non sqrt version
        (loop (crndvec))
        v)))

;; StartFunctionDoc-en
;; hsrndvec
;; Returns: vector
;; Description:
;; Returns a random 3 element vector. If you visualise a lot of these as
;; points, they will cover the surface of a sphere centred on the origin
;; (see the example). The name stands for "hollow sphere".
;; Example:
;; (clear)
;; (hint-none)
;; (hint-points)
;; (point-width 4)
;; (define p (build-particles 1000))
;;
;; (show-axis 1)
;;
;; (with-primitive p
;;     (pdata-map!
;;         (lambda (p)
;;             (vector 1 1 1))
;;         "c")
;;     (pdata-map!
;;         (lambda (p)
;;             (hsrndvec))
;;         "p"))
;; EndFunctionDoc

(define (hsrndvec)
  (let loop ((v (crndvec)))
    (let ((l (vmag v)))
      (if (or (> l 1) (eq? l 0))
          (loop (crndvec))
          (vdiv v l)))))


;; StartFunctionDoc-en
;; grndf
;; Returns: number
;; Description:
;; Returns a gaussian random number in the range centred on zero, with a variance of 1
;; Example:
;; (display (grndf))(newline)
;; EndFunctionDoc

;; gaussian
(define (grndf)
  (let loop ((x (crndf)) (y (crndf)))
    (let ((l (+ (* x x) (* y y))))
      (if (or (>= l 1) (eq? l 0))
          (loop (crndf) (crndf))
          (* (sqrt (/ (* -2 (log l)) l)) x)))))

;; StartFunctionDoc-en
;; grndvec
;; Returns: vector
;; Description:
;; Returns a gaussian random 3 element vector. If you visualise a lot of
;; these as points, you will see a normal distribution centred on the origin.
;; (see the example).
;; Example:
;; (clear)
;; (hint-none)
;; (hint-points)
;; (point-width 4)
;; (define p (build-particles 1000))
;;
;; (show-axis 1)
;;
;; (with-primitive p
;;     (pdata-map!
;;         (lambda (p)
;;             (vector 1 1 1))
;;         "c")
;;     (pdata-map!
;;         (lambda (p)
;;             (grndvec))
;;         "p"))
;; EndFunctionDoc

(define (grndvec)
  (vector (grndf) (grndf) (grndf)))

;; StartFunctionDoc-en
;; rndbary
;; Returns: vector
;; Description:
;; Returns a vector representing a uniformly distributed triangular
;; barycentric coordinate (wip - doesn't seem to be very uniform to me...)
;; Example:
;; (rndbary)
;; EndFunctionDoc

(define (rndbary)
	(let*
		((a (- 1.0 (sqrt (rndf))))
		 (b (* (rndf) (- 1.0 a)))
		 (c (- 1.0 (+ a b))))
		(vector a b c)))

;; StartFunctionDoc-en
;; rndbary normal
;; Returns: vector
;; Description:
;; Returns a vector representing a random point on a hemisphere, defined by normal.
;; Example:
;; (clear)
;; (hint-none)
;; (hint-points)
;; (point-width 4)
;; (define p (build-particles 1000))
;;
;; (show-axis 1)
;;
;; (with-primitive p
;;     (pdata-map!
;;         (lambda (p)
;;             (vector 1 1 1))
;;         "c")
;;     (pdata-map!
;;         (lambda (p)
;;             (rndhemi (vector 0 1 0)))
;;         "p"))
;; EndFunctionDoc

; return a line on the hemisphere
(define (rndhemi n)
  (let loop ((v (srndvec)))
    (if (> (vdot n v) 0)
        v
        (loop (srndvec)))))


;; StartFunctionDoc-en
;; hrndbary normal
;; Returns: vector
;; Description:
;; Returns a vector representing a random point on a hollow hemisphere, defined by normal.
;; Example:
;; (clear)
;; (hint-none)
;; (hint-points)
;; (point-width 4)
;; (define p (build-particles 1000))
;;
;; (show-axis 1)
;;
;; (with-primitive p
;;     (pdata-map!
;;         (lambda (p)
;;             (vector 1 1 1))
;;         "c")
;;     (pdata-map!
;;         (lambda (p)
;;             (hrndhemi (vector 0 1 0)))
;;         "p"))
;; EndFunctionDoc

; return a line on the hemisphere
(define (hrndhemi n)
  (let loop ((v (hsrndvec)))
    (if (> (vdot n v) 0)
        v
        (loop (hsrndvec)))))


)

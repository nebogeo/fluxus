(require frtime/animation
         frtime/list)

(define player-pos
  (make-posn
   (+ 200 (integral
           (hold
            (map-e
             (lambda (key)
               (if (equal? key #\z) -0.1
                   (if (equal? key #\x) 0.1)))
             key-strokes) 0)))
   350))

(define fire-time-e
    (map-e
     (lambda (_)
       (value-now milliseconds))
     (filter-e
      (lambda (k)
        (eq? k #\ ))
      key-strokes)))
 
(define (truncate-list lst count)
  (cond 
    ((zero? count) '())
    ((null? lst) '())
    (else (cons (car lst) (truncate-list (cdr lst) (- count 1))))))

(define (factory proc event max-size)
    (collect-b
     event '()
     (lambda (e lst)
       (cons (proc e) (truncate-list lst max-size)))))

(define (collide? pos with-list radius)
 (foldl
   (lambda (with collided)
     (if collided collided
         (if (solid-ellipse? with)
             (< (posn-diff pos (solid-ellipse-ul with)) radius)
             collided)))
   #f
   with-list))

(define bullets
  (factory
   (lambda (t)
     (if (< milliseconds (+ t 2000))
         (make-circle (make-posn (posn-x (value-now player-pos))
                                 (+ (posn-y (value-now player-pos))
                                    (integral -0.2))) 2 "red")))        
   fire-time-e 5))

(define (make-grid w h offset mul)
  (let yloop ((y h) (l '()))
    (cond 
      ((zero? y) l)
      (else 
       (yloop (- y 1)
              (let xloop ((x w) (l l))
                (cond 
                  ((zero? x) l)
                  (else
                   (xloop (- x 1) 
                          (cons (posn+ offset (posn* (make-posn x y) mul))
                                l))))))))))

(display-shapes
 (list
  bullets
  (map
   (lambda (pos)
     (if (not (hold (when-e (collide? pos bullets 10)) #f))
         (make-circle pos 15 "green")))
   (make-grid 5 5  (make-posn 50 20) 50))

  (make-circle player-pos 10 "blue")
  (make-graph-string (make-posn 10 20) "FrTime Invaders!" "red")
  (make-graph-string (make-posn 10 35) "Press \"z\" to move left, \"x\" to move right and space to fire" "red")))
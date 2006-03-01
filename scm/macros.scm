(define-module (fluxus macros))
(export every-frame)

(use-syntax (ice-9 syncase)) ; for R5RS macros

(define-syntax every-frame
  (syntax-rules ()
    ((every-frame expr1 ...)
     (let ((h (frame-hook)))
       (add-hook! h
          (lambda () expr1 ...))))))


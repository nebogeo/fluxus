; Copyright (C) 2007 Dave Griffiths
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; StartSectionDoc-en
;; scratchpad-help
;; The help system is fluxus' (slightly experimental) online documentation.
;; Example:
;; EndSectionDoc 

#lang scheme/base

(provide 
 init-help
 help
 set-help-locale!)

(define helpmap-all-locales '())
(define helpmap '())
(define fluxus-scratchpad-locale "en")

; The helpmap is of the form:
;
; (("SectionName" ("SectionDescription" "SectionExample"
;            (("Function name" ("Arg list" "Description" "Example")) 
;              ...)))
;     (...))
;
; While the helpmap-all-locales is of the form:
;
; (("locale" helpmap)
;	 ("locale" helpmap)
;  ...)	

;; StartFunctionDoc-en
;; set-help-locale! locale-string
;; Returns: void
;; Description:
;; Sets the language for the documentation
;; Example:
;; (set-help-locale! "pt") 
;; EndFunctionDoc 

;; StartFunctionDoc-pt
;; set-help-locale! string-locale
;; Retorna: void
;; Descrição:
;; Ajusta a linguagem para a documentação.
;; Exemplo:
;; (set-help-locale! "en")
;; EndFunctionDoc

(define (set-help-locale! locale)
  (let ((newhelpmap (assoc locale helpmap-all-locales)))
    (cond
      (newhelpmap (set! helpmap (cadr newhelpmap)))
      (else (display "locale \"")(display locale)(display " not found...")(newline)))))

;; StartFunctionDoc-en
;; help function-string
;; Returns: void
;; Description:
;; Displays help information on a fluxus function. For running in the repl mainly.
;; Example:
;; (help "pop") 
;; EndFunctionDoc    

;; StartFunctionDoc-pt
;; help string-função
;; Retorna: void
;; Descrição:
;; Mostra informação de ajuda numa função do fluxus. Para rodar no
;; repl principalmente.
;; Exemplo:
;; (help "pop")
;; EndFunctionDoc

(define (help . args)
  (if (null? args) ; help without an argument prints out some 
    (func-help-example "tophelp")   ; top level help information (defined above)
    (cond
      ((string=? (car args) "keys") (func-help-example (car args)))
      ((string=? (car args) "console") (func-help-example (car args)))
      ((string=? (car args) "editor") (func-help-example (car args)))
      ((string=? (car args) "camera") (func-help-example (car args)))
      ((string=? (car args) "language") (func-help-example (car args)))
      ((string=? (car args) "sections") (section-summary))
      ((string=? (car args) "misc") (func-help-example (car args)))
      ((string=? (car args) "toplap") (func-help-example (car args)))
      ((string=? (car args) "authors") (func-help-example (car args)))
      (else
       (when (not (section-help (car args))) ; first look in the section help
         (func-help (car args)))))))     ; if it fails fall through to function help

(define (insert-linebreaks src dst count i n)
  (if (>= i (string-length src))
      dst
      (if (and (> n count) (char=? (string-ref src i) #\space))
          (insert-linebreaks src 
                             (string-append dst (string (string-ref src i)) (string #\newline)) count (+ i 1) 0)
          (insert-linebreaks src 
                             (string-append dst (string (string-ref src i))) count (+ i 1) (+ n 1)))))

;; Dave could we have a different version of this function (func-help) for each locale? --greb
;; I've simplified the output, but we need to sort out how to cope this these smaller bits of 
;; text - the save-as dialog is another example. --dave

(define (func-help funcname)  
  (define (inner-help l)
    (let ((ret (assoc funcname (list-ref (cadr (car l)) 2))))
      (cond
        (ret
         (display "(")(display (car ret))
         (let ((arguments (list-ref (list-ref ret 1) 0)))
           (cond 
             ((not (zero? (string-length arguments)))
              (display " ")
              (display arguments))))
         (display ")")(newline)(newline)
         (display "Returns ")
         (display (list-ref (list-ref ret 1) 1))(newline)(newline)
         (display (insert-linebreaks (list-ref (list-ref ret 1) 2) "" 50 0 0))
         (newline)(newline)
         (display (list-ref (list-ref ret 1) 3))
         (newline))
        (else
         (if (null? (cdr l))
             "Function not found"
             (inner-help (cdr l)))))))
  (cond 
    ((null? helpmap)
     (display "No helpmap exists...")(newline)
     (display "Try running \"makedocs.sh\" in the fluxus docs directory")(newline))
    (else
     (inner-help helpmap))))

; just print out the example for (preformatted) documentation which isn't
; really a function - need to do this in a more general way...!
(define (func-help-example funcname)  
  (define (inner-help l)
    (let ((ret (assoc funcname (list-ref (cadr (car l)) 2))))
      (cond
        (ret
         (display (list-ref (list-ref ret 1) 3))
         (newline))
        (else
         (if (null? (cdr l))
             "Function not found"
             (inner-help (cdr l)))))))
  (cond 
    ((null? helpmap)
     (display "No helpmap exists...")(newline)
     (display "Your fluxus installation is not setup properly.")(newline)
     (display "Try running \"makedocs.sh\" in the fluxus docs directory,")(newline)
     (display "and reinstalling with \"sudo scons install\"")(newline))
    (else
     (inner-help helpmap))))

; print out the section summary
(define (section-summary)  
  (define (print-section section)
    (display (car section))(newline))
  
  (define (inner-summary l)
    (cond ((null? l) 0)
          (else
           (print-section (car l))
           (inner-summary (cdr l))))) 
  (cond 
    ((null? helpmap)
     (display "No helpmap exists...")(newline)
     (display "Try running \"makedocs.sh\" in the fluxus docs directory")(newline)
     (display "and reinstalling with \"sudo scons install\"")(newline))
    (else
     (inner-summary helpmap))))

; print out the section info
(define (section-help sectionname)  
  
  (define (print-functions l c)
    (cond 
      ((null? l) 0)
      (else
       (display (car (car l)))(display " ")
       (when (zero? (modulo c 5)) (newline)) ; crude wrapping
       (print-functions (cdr l) (+ c 1)))))
  
  (define (print-section section)
    (display "Section: ")
    (display (car section))(newline) ; section name
    (display (insert-linebreaks (car (cadr section)) "" 50 0 0))(newline)(newline) ; section desc
    (display (cadr (cadr section)))(newline) ; section example
    (display "Functions:")(newline)
    (print-functions (caddr (cadr section)) 1)(newline) ; functions
    #t) 
  
  (define (inner-help l)
    (let ((section (assoc sectionname l)))
      (cond
        (section
         (print-section section))
        (else
         #f))))
  
  (cond 
    ((null? helpmap)
     (display "No helpmap exists...")(newline)
     (display "Try running \"makedocs.sh\" in the fluxus docs directory")(newline)
     (display "and reinstalling with \"sudo scons install\"")(newline)
     #f)
    (else
     (inner-help helpmap))))

(define (init-help helpmapfile)
  (cond 
    ((file-exists? helpmapfile)
     (let ((file (open-input-file helpmapfile)))
       (set! helpmap-all-locales (read file))
       (close-input-port file)
       (set-help-locale! "en"))))) ; default to english... 



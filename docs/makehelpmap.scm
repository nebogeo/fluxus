#!/usr/bin/env mzscheme 

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

;; auto documentation generator for fluxus
;; 
;; parses C++ and Scheme files looking for function documentation
;; and then converts this into a scheme helpmap list 

#lang racket/base

(require scheme/file)
(require scheme/path)

(provide 
 makehelpmap
 write-helpmapfile
 set-locale)

; print out debug info
(define debug-mode #t)

(define (debug data)
  (cond 
    (debug-mode
     (display (car data))
     (if (null? (cdr data))
         (newline)
         (debug (cdr data))))))

; tells us which language to extract documentation for
(define locale "pt")

; the tags to use for each language - override them 
; in set-locale
(define locale-returns "Returns:")
(define locale-description "Description:")
(define locale-example "Example:")

(define (set-locale s)
  (debug (list "setting locale to:" s))
  (set! locale s)
  (cond 
    ((string=? s "pt")
     (set! locale-returns "Retorna:")
     (set! locale-description "Descrição:")
     (set! locale-example "Exemplo:"))
    ((string=? s "en")
     (set! locale-returns "Returns:")
     (set! locale-description "Description:")
     (set! locale-example "Example:"))
    ((string=? s "fr")
     (set! locale-returns "Retour:")
     (set! locale-description "Description:")
     (set! locale-example "Exemple:"))))

; returns a string of characters from the current position
; using the delimit function to mark beginning and end
(define (read-token file delimit-fn)
  (define (skip-delimiter)
    (let ((c (peek-char file)))
      (cond 
        ((eof-object? c)
         0)
        ((not (delimit-fn c))
         0)
        (else
         (read-char file)
         (skip-delimiter)))))
  
  (define (inner-read-token str)
    (let ((c (read-char file)))
      (cond
        ((eof-object? c)
         str)
        ((delimit-fn c)
         str)
        (else
         (inner-read-token (string-append str (string c)))))))
  
  (when (delimit-fn (peek-char file)) 
    (skip-delimiter))
  (inner-read-token ""))

; read token specialisation for words
(define (read-word file)
  (read-token file (lambda (c) (char-whitespace? c))))

; read token specialisation for words
(define (read-word-newline file)
  (read-token file (lambda (c) (or (char=? c #\space) (char=? c #\tab)))))

; look for description tokens
(define (functiondoc? file)
  (string=? (read-word file) (string-append "StartFunctionDoc-" locale)))

(define (sectiondoc? file)
  (string=? (read-word file) (string-append "StartSectionDoc-" locale)))

; gather all words up to end word
(define (gather file end)
  (define (inner-gather str)
    (let ((w (read-word file)))
      (cond 
        ((string=? w end)
         str)
        ((string=? w "//") ; ignore C++ comments 
         (inner-gather str))
        ((string=? w ";;") ; ignore Scheme comments 
         (inner-gather str))
        ((string=? str "")
         (inner-gather (string-append w)))
        (else
         (inner-gather (string-append str " " w))))))
  
  (debug (list "Gather - looking for: " end))
  (inner-gather ""))

; gather all words up to end word including newlines
(define (gather-formatted file end)
  
  (define (check-end-string str)
    (let ((c (read-char file)))
      (cond 
        ((char-whitespace? c)
         (if (string=? str end)
             #t
             str))
        (else
         (check-end-string (string-append str (string c)))))))
  
  (define (inner-gather str first-char)
    (let ((c (read-char file)))
      (cond
        ((eof-object? c)
         str)
        ((char=? c first-char)
         (let ((next (check-end-string (string c))))
           (if (string? next)
               (inner-gather (string-append str next) first-char)
               str)))
        (else
         (inner-gather (string-append str (string c)) first-char)))))
  
  (define (remove-comments str)
    (define (inner-remove-comments in out)
      (if (or (null? (cdr in)) (null? (cddr in)))
          out
          (if (or 
               (and (char=? (car in) #\;)
                    (char=? (car (cdr in)) #\;))
               (and (char=? (car in) #\/)
                    (char=? (car (cdr in)) #\/)))
              (inner-remove-comments (cddr in) out)
              (inner-remove-comments (cdr in) (append out (list (car in)))))))
    
    (list->string (inner-remove-comments (string->list str) '())))
  
  (debug (list "Gather formatted, looking for: " end))
  (let ((first-char (car (string->list end))))
    (remove-comments (inner-gather "" first-char))))

; extracts the data we need from the C++ file and 
; adds it to the input list of strings per function
; in the form:
; ((funcname (arguments description example)))

(define (parse-functions file l)
  (let ((c (peek-char file)))
    (if (eof-object? c)
        l
        (cond 
          ((not (functiondoc? file))
           (parse-functions file l))
          (else
           (read-word file) ; comment //
           (let ((funcname (read-word file)))
             (debug (list funcname))
             (parse-functions file (append l (list (list
                                                    funcname 
                                                    (list
                                                     (gather file locale-returns)
                                                     (gather file locale-description)
                                                     (gather file locale-example)
                                                     (gather-formatted file "EndFunctionDoc"))))))))))))

(define (section-insert l name section functions o app)
  (cond
    ((null? l) (if app (append o (list (list name (append section (list functions))))) o))
    ((string=? (car (car l)) name)
     (printf "~a~n" (list-ref (car (cdr (car l))) 2))
     (section-insert (cdr l) name section functions
                     (cons (list name (append section (list (append (list-ref (car (cdr (car l))) 2) functions)))) o) #f))
    (else 
     (section-insert (cdr l) name section functions
                     (cons (car l) o) app))))

(define (parse-section file l)
  (let ((c (peek-char file)))
    (if (eof-object? c)
        l
        (cond 
          ((not (sectiondoc? file))
           (parse-section file l))
          (else
           (read-word file) ; comment //
           (let ((name (read-word file))
                 (section (list (gather file locale-example)
                                (gather-formatted file "EndSectionDoc"))))
             
             (section-insert l name section (parse-functions file '()) '() #t)))))))

(define (parse-file inputfilename helpmap)
  (let ((file (open-input-file inputfilename)))
    (let ((t (parse-section file helpmap)))
      (close-input-port file)
      t)))

(define (parse-files inputfilenames helpmap)
  (if (null? inputfilenames)
      helpmap
      (parse-files (cdr inputfilenames) (parse-file (car inputfilenames) helpmap))))

(define (write-helpmapfile filename helpmap)
  ;(display (string-append "deleting " filename))(newline)
  ;    (delete-file filename)
  (let ((helpmapfile (open-output-file filename #:exists 'replace)))
    (write helpmap helpmapfile)
    (close-output-port helpmapfile)))

(define (makehelpmap file helpmap)
  (debug (list "parsing " file))
  (parse-file file helpmap))


(define (recurse-dir locale path visitor helpmap)
  (define (inner l helpmap)
    (cond 
      ((null? l) helpmap)
      (else
       (let ((newpath (build-path path (car l))))
         (cond 
           ((directory-exists? newpath)
            (inner (cdr l) (recurse-dir locale (path->string newpath) visitor helpmap)))
                    (else
                         (inner (cdr l) (visitor newpath helpmap))))))))
  
  (let ((pathlist (directory-list path)))
    (cond 
      ((null? pathlist) helpmap)
      (else
       (set-locale locale)
       (inner pathlist helpmap)))))

(define (visitor path helpmap)
  (let ((extn (filename-extension path)))
    (if (not extn)
        helpmap
        (cond 
          ((string=? (bytes->string/utf-8 extn) "scm")
           (makehelpmap path helpmap))
          ((string=? (bytes->string/utf-8 extn) "ss")
           (makehelpmap path helpmap))
          ((string=? (bytes->string/utf-8 extn) "cpp")
           (makehelpmap path helpmap))
          (else helpmap)))))

(define (gather-locales path localelist helpmap)
  (cond
    ((null? localelist) helpmap)
    (else (gather-locales path (cdr localelist) 
                          (cons (list (car localelist) 
                                      (recurse-dir (car localelist) path visitor '())) helpmap)))))

(write-helpmapfile "helpmap.scm" (gather-locales "../" '("pt" "en" "fr") '()))




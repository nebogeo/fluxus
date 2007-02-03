#!/usr/local/bin/mzscheme -r
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

(module makehelpmap mzscheme
  (provide 
   makehelpmap
   write-helpmapfile)
  
  ; tells us which language to extract documentation for
  (define locale "en")
  
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
    
    (if (delimit-fn (peek-char file)) 
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
			 ;(display funcname)(newline)
             (parse-functions file (append l (list (list
                                                    funcname 
                                                    (list
                                                     (gather file "Returns:")
                                                     (gather file "Description:")
                                                     (gather file "Example:")
                                                     (gather-formatted file "EndFunctionDoc"))))))))))))
  
  (define (parse-section file l)
    (let ((c (peek-char file)))
      (if (eof-object? c)
          l
          (cond 
            ((not (sectiondoc? file))
             (parse-section file l))
            (else
             (read-word file) ; comment // 
             (append l (list (list
                              (read-word file) ; section name
                              (list
                               (gather file "Example:")
                               (gather-formatted file "EndSectionDoc")
                               (parse-functions file '()))))))))))
  
  (define (parse-file inputfilename helpmap)
    (let ((file (open-input-file inputfilename)))
      (let ((t (parse-section file helpmap)))
        (close-input-port file)
        t)))
  
  (define (parse-files inputfilenames helpmap)
    (if (null? inputfilenames)
        helpmap
        (parse-files (cdr inputfilenames) (parse-file (car inputfilenames) helpmap))))
  
  (define helpmaplist '())
  
  (define (write-helpmapfile filename)
    ;(display (string-append "deleting " filename))(newline)
    ;    (delete-file filename)
    (let ((helpmapfile (open-output-file filename)))
      (write helpmaplist helpmapfile)
      (close-output-port helpmapfile)))
  
  (define (makehelpmap file)
    ;(display "parsing ")(display file)(newline)
    (set! helpmaplist (parse-file file helpmaplist)))
  
  ) ; module makehelpmap

(require makehelpmap)
(require (lib "file.ss"))

(define (recurse-dir path visitor)
  (define (inner l)
    (let ((newpath (build-path path (car l))))
      (cond 
        ((directory-exists? newpath)
         (recurse-dir (path->string newpath) visitor))
        (else
         (visitor newpath)))
      (if (null? (cdr l))
          0
          (inner (cdr l)))))
  (let ((pathlist (directory-list path)))
    (if (not (null? pathlist))
        (inner (directory-list path)))))

(define (visitor path)
  (let ((extn (filename-extension path)))
    (if extn
        (cond 
          ((string=? (bytes->string/utf-8 extn) "scm")
           (makehelpmap path))
          ((string=? (bytes->string/utf-8 extn) "ss")
           (makehelpmap path))
          ((string=? (bytes->string/utf-8 extn) "cpp")
           (makehelpmap path))))))

(cond 
  ((eq? (vector-length (current-command-line-arguments)) 2)
   (let ((args (vector->list (current-command-line-arguments))))
     (recurse-dir (car args) visitor)
     (write-helpmapfile (car (cdr args)))))
  (else
   (display "usage: makehelpmap.scm path helpfile")(newline)))



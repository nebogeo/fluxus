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
;; (c) 2006 Dave Griffiths 
;; 
;; parses helpmap scheme list files and converts all the
;; functions into texi formatted text

(module help->texi mzscheme
  (provide help->texi)
  
  (define (build-menu helpmap output)  
    (display "* " output)
    (display (caar helpmap) output)
    (display ":: " output)
    (display (caar helpmap) output)
    (display #\newline output)
    (if (null? (cdr helpmap))
        0
        (build-menu (cdr helpmap) output))) 
  
  (define (parse-helpmap helpmap output)
    (parse-section (car helpmap) output)
    (if (null? (cdr helpmap))
        0
        (parse-helpmap (cdr helpmap) output)))
  
  (define (parse-section section output)
    (display "@node " output)
    (display (car section) output)
    (display #\newline output)
    (display "@chapter " output)
    (display (car section) output)
    (display #\newline output)
    (display (list-ref (cadr section) 0) output)
    (display #\newline output)
    (display "@subsection Example" output)
    (display #\newline output)
    (display "@lisp" output)
    (display #\newline output)
    (display (list-ref (cadr section) 1) output)
    (display #\newline output)
    (display "@end lisp" output)
    (display #\newline output)
    (parse-functions (list-ref (cadr section) 2) output))
  
  (define (parse-functions funcmap output)
    (display "@section (" output)
    (display (car (car funcmap)) output)
    (let ((arguments (list-ref (list-ref (car funcmap) 1) 0)))
      (cond 
        ((not (zero? (string-length arguments)))
         (display " " output)
         (display arguments output))))
    (display ")" output)
    (display #\newline output)
    
    (display "@subsection Returns" output)
    (display #\newline output)
    (display (list-ref (list-ref (car funcmap) 1) 1) output)
    (display #\newline output)
    
    (display "@subsection Description" output)
    (display #\newline output)
    (display (list-ref (list-ref (car funcmap) 1) 2) output)
    (display #\newline output)
    
    (display "@subsection Example" output)
    (display #\newline output)
    (display "@lisp" output)
    (display #\newline output)
    (display (list-ref (list-ref (car funcmap) 1) 3) output)
    (display #\newline output)
    (display "@end lisp" output)
    (display #\newline output)
    
    
    (display #\newline output)
    (if (null? (cdr funcmap))
        0
        (parse-functions (cdr funcmap) output)))
  
  (define (help->texi helpmapfilename texifilename boilerplatefilename)
    (let ((file (open-input-file helpmapfilename))
          (outfile (open-output-file texifilename)))
      
      (let ((boilerplate (open-input-file boilerplatefilename)))
        (display (read boilerplate) outfile)
        (display #\newline outfile)
        (close-input-port boilerplate))
      
      (let ((helpmap (read file)))
        
        (display "@menu" outfile)
        (display #\newline outfile)
        (build-menu helpmap outfile)    
        (display "@end menu" outfile)
        (display #\newline outfile)
        (parse-helpmap helpmap outfile)
        (close-output-port outfile))
      (close-input-port file)))
  
  ) ; module help->texi

(require help->texi)
(help->texi "helpmap.scm" "fluxus.texi" "boilerplate.scm")

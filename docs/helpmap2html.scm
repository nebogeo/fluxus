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

; a script to convert the fluxus helpmap into a human readable text file
; fixed by Claude Heiland-Allen 

#lang scheme/base

(require scheme/file)
(require scheme/path)

(define (clean-id name) (regexp-replace "[!?<>]" name "_"))
(define (clean-text text) (regexp-replace ">" (regexp-replace "<" (regexp-replace "&" text "\\&amp;") "\\&lt;") "\\&gt;"))

(define (write-header locale title htmlfile cssfilename)
  (define (line-loop in out)
    (let ((ret (read-line in)))
      (cond 
        ((not (eof-object? ret))
         (fprintf out "~a~n" ret)
         (line-loop in out)))))

  (fprintf htmlfile "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"~a\" lang=\"~a\">~n<head>" locale locale)
  (fprintf htmlfile "<title>~a</title>~n" (clean-text title))
  (fprintf htmlfile "<style type=\"text/css\"><!--~n")
  (let ((cssfile (open-input-file cssfilename)))
    (line-loop cssfile htmlfile)
    (close-input-port cssfile))
  (fprintf htmlfile "--></style>~n</head>~n<body>~n"))
  
(define (write-footer htmlfile)
  (fprintf htmlfile "</body>~n</html>~n"))

(define (write-function name args returns desc example htmlfile)
  (if (string=? args "")
      (fprintf htmlfile "<h2><a name=\"~a\">(~a)</a></h2>" (clean-id name) (clean-text name))
      (fprintf htmlfile "<h2><a name=\"~a\">(~a ~a)</a></h2>~n" (clean-id name) (clean-text name) (clean-text args)))
  (fprintf htmlfile "<p><b>Returns</b> ~a</p>~n" (clean-text returns))
  (fprintf htmlfile "<p>~a</p>~n" (clean-text desc))
  (fprintf htmlfile "<p><b>Example</b></p>~n")
  (fprintf htmlfile "<pre>~a</pre>~n" (clean-text example)))

(define (write-section name desc example htmlfile)
  (fprintf htmlfile "<h1>~a</h1>~n" (clean-text name))
  (fprintf htmlfile "<h3>Description</h3>~n")
  (fprintf htmlfile "<p>~a</p>~n" (clean-text desc))
  (cond 
    ((not (string=? example ""))
     (fprintf htmlfile "<h3>Example</h3>~n")
     (fprintf htmlfile "<pre>~a</pre>~n" (clean-text example)))))

(define (write-section-index name htmlfile)
  (fprintf htmlfile "<h2><a href=\"~a.html\">~a</a></h2>" name (clean-text name)))

(define (write-locale locale htmlfile)
  (fprintf htmlfile "<a href=\"~a/index.html\">~a</a> " locale locale))

(define (write-all-function name section htmlfile)
  (fprintf htmlfile "<a href=\"~a.html#~a\">~a</a> " section (clean-id name) (clean-text name)))

(define (write-functionlist-start htmlfile)
  (fprintf htmlfile "<p>"))

(define (write-functionlist-end htmlfile)
  (fprintf htmlfile "</p>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-function functionlist htmlfile allhtmlfile section)
  (cond
    ((not (null? functionlist))
     (let ((function (cadr (car functionlist))))
       (write-all-function (car (car functionlist)) section allhtmlfile)
       (write-function (car (car functionlist)) 
                       (car function) 
                       (cadr function) 
                       (caddr function) 
                       (cadddr function) 
                       htmlfile))
     (parse-function (cdr functionlist) htmlfile allhtmlfile section))))

(define (parse-section sectionlist indexhtmlfile locale)
  (cond
    ((not (null? sectionlist))
     (let ((section (cadr (car sectionlist)))
           (sectionname (car (car sectionlist))))
       (write-section-index sectionname indexhtmlfile)
       (let ((htmlfile (open-output-file (string-append locale "/" sectionname ".html") #:exists 'replace)))
         (write-header locale sectionname htmlfile "fluxusdoc.css")
         (write-section sectionname (car section) (cadr section) htmlfile)
         (write-functionlist-start indexhtmlfile)
         (parse-function (caddr section) htmlfile indexhtmlfile sectionname)
         (write-functionlist-end indexhtmlfile)
         (write-footer htmlfile)
         (close-output-port htmlfile)))
     (parse-section (cdr sectionlist) indexhtmlfile locale))))
 
(define (parse-locale helpmap htmlfile)
  (cond
    ((not (null? helpmap))
     (let ((locale (car (car helpmap))))
       (when (not (directory-exists? locale))
           (make-directory locale))
       (write-locale locale htmlfile)
       (let ((htmlfile (open-output-file (string-append locale "/index.html") #:exists 'replace)))
         (write-header locale "Section Index" htmlfile "fluxusdoc.css")
         (parse-section (cadr (car helpmap)) htmlfile locale)
         (write-footer htmlfile)
         (close-output-port htmlfile))
       (parse-locale (cdr helpmap) htmlfile)))))


(let ((file (open-input-file "helpmap.scm")))
  (let ((htmlfile (open-output-file "index.html" #:exists 'replace)))
    (write-header "en" "Fluxus Documentation 0.15" htmlfile "fluxusdoc.css")
    (fprintf htmlfile "<h1>Fluxus Documentation 0.15</h1>~n")
    (fprintf htmlfile "<p>Extracted and html-ised from the runtime documentation system</p>~n")
    (fprintf htmlfile "<p>Available languages: ")
    (parse-locale (read file) htmlfile)
    (close-input-port file)
    (fprintf htmlfile "</p>")
    (write-footer htmlfile)
    (close-output-port htmlfile)))

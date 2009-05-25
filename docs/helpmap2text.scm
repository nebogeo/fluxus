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

#lang scheme/base

(require scheme/file)
(require scheme/path)

(define (insert-linebreaks src dst count i n)
  (if (>= i (string-length src))
      dst
      (if (and (> n count) (char=? (string-ref src i) #\space))
          (insert-linebreaks src 
                             (string-append dst (string (string-ref src i)) (string #\newline)) count (+ i 1) 0)
          (insert-linebreaks src 
                             (string-append dst (string (string-ref src i))) count (+ i 1) (+ n 1)))))


(define (write-function name args returns desc example txtfile)
  (fprintf txtfile "------------------------------------------------------------~n~n")
  (if (string=? args "")
      (fprintf txtfile "(~a)~n" name)
      (fprintf txtfile "(~a ~a)~n" name args))
  (when (not (string=? returns ""))
      (fprintf txtfile "Returns ~a~n~n" returns))
  (fprintf txtfile "~a~n~n" (insert-linebreaks desc "" 60 0 0))
  (fprintf txtfile "~a~n" example))

(define (write-section name desc example txtfile)
  (fprintf txtfile "============================================================~n~n")
  (fprintf txtfile "Section: \"~a\"~n~n" name)
  (fprintf txtfile "~a~n~n" (insert-linebreaks desc "" 60 0 0))
  (cond 
    ((not (string=? example ""))
     (fprintf txtfile "~a~n" example))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-function functionlist txtfile section)
  (cond
    ((not (null? functionlist))
     (let ((function (cadr (car functionlist))))     
       (write-function (car (car functionlist)) 
                       (car function) 
                       (cadr function) 
                       (caddr function) 
                       (cadddr function) 
                       txtfile))
     (parse-function (cdr functionlist) txtfile section))))

(define (parse-section sectionlist txtfile locale)
  (cond
    ((not (null? sectionlist))
     (let ((section (cadr (car sectionlist)))
           (sectionname (car (car sectionlist))))
       (write-section sectionname (car section) (cadr section) txtfile)
       (parse-function (caddr section) txtfile sectionname))
     (parse-section (cdr sectionlist) txtfile locale))))
 
(define (parse-locale helpmap)
  (cond
    ((not (null? helpmap))
     (let ((locale (car (car helpmap))))
       (let ((txtfile (open-output-file (string-append "fluxus-function-ref-" locale ".txt") #:exists 'replace)))
         (fprintf txtfile "Fluxus Documentation 0.16~n")
         (parse-section (cadr (car helpmap)) txtfile locale)
         (close-output-port txtfile))
       (parse-locale (cdr helpmap))))))


(let ((file (open-input-file "helpmap.scm")))
  (parse-locale (read file)))

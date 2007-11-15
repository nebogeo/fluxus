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

(module fluxus-collada-import mzscheme
  (require "fluxus-engine.ss")
  (require (lib "fluxus-building-blocks.ss" "fluxus-0.14"))
  (require (lib "xml.ss" "xml"))
  (provide
   collada-load
   )
  
  ;;--------------------------------------------------------------------
  ;; xml parsing nonsense...
  
  ;; returns a list of subelements of element x
  (define (element-list x)
    (define (subelement-name x)
      (car x))
    (define (subelement-attributes x)
      (cadr x))
    (define (subelement x)
      (cddr x))
    (letrec ((loop 
              (lambda (x out)
                (cond
                  ((null? x) out)
                  ((not (string? (car x)))                 
                   (loop (cdr x) (cons (list 
                                        (subelement-name (car x))
                                        (subelement-attributes (car x))                                       
                                        (element-list (subelement (car x)))
                                        (if (and (not (null? (subelement (car x))))
                                                 (string? (car (subelement (car x)))))
                                            (subelement (car x))
                                            '())
                                        ) out)))  
                  (else
                   (loop (cdr x) out))))))
      (loop x '())))
  
  ;; accessors for members of element-list
  (define (get-data e)
    (car (cdr (cdr (cdr e)))))
  
  ;; accessors for members of element-list
  (define (get-elements e)
    (car (cdr (cdr e))))
  
  ;; accessors for members of element-list
  (define (get-attributes e)
    (car (cdr e)))
  
  ;; accessors for members of element-list
  (define (get-name e)
    (car e))
  
  
  ;; does an element exist with this name?
  (define (element-exists? x name)
    (letrec 
        ((find 
          (lambda (x)
            (cond
              ((null? x) #f)
              ((and (not (string? (car x)))
                    (eq? (get-name (car x)) name))
               #t)
              (else
               (find (cdr x)))))))
      (find (get-elements x))))
  
  
  ;; look for an element with a specific name and return the first one found
  (define (find-element x name)
    (letrec ((find (lambda (x)
                     (cond
                       ((null? x) (error "fluxus-collada-import: could'nt find element called " name))
                       ((eq? (get-name (car x)) name)
                        (car x))
                       (else
                        (find (cdr x)))))))
      (find (get-elements x))))
  
  (define (find-element-temp x name)
    (letrec ((find (lambda (x)
                     (cond
                       ((null? x) (error "fluxus-collada-import: could'nt find element called " name))
                       ((eq? (get-name (car x)) name)
                        (car x))
                       (else
                        (find (cdr x)))))))
      (find x)))
  
  ;; get the value of the name in some metadata
  (define (get-attribute x name)
    (define (get x name)
      (cond 
        ((null? x) (error "fluxus-collada-import: couldn't find attribute called " name))
        ((eq? (car (car x)) name)
         (car (cdr (car x))))
        (else 
         (get (cdr x) name))))
    (get (get-attributes x) name))
  
  ;; look for elements with name and attribute token/value match
  (define (match-elements x name attribute-name attribute-value)
    (letrec 
        ((loop 
          (lambda (x out)
            (cond
              ((null? x) out)
              ((eq? (get-name (car x)) name)
               (let ((t (get-attribute (car x) attribute-name)))
                 (if (equal? t attribute-value)
                     (loop (cdr x) (cons (car x) out))
                     (loop (cdr x) out))))
              (else
               (loop (cdr x) out))))))
      (loop (get-elements x) '())))
  
  (define (element-match-exists? x name attribute-name attribute-value)
    (not (null? (match-elements x name attribute-name attribute-value))))
  
  ;; use a list of element names to drill down like a path
  (define (get-element-from-path x path)
    (cond 
      ((null? path) x)
      (else
       (get-element-from-path (find-element x (car path)) (cdr path)))))
  
  ;;--------------------------------------------------------
  ;; building geometry
  ;; this is the fluxus interface
  
  ;; topology is the poly type as a symbol - check it's the same as fluxus type
  ;; format describes the contents of data in terms of pdata type eg: ("p" "n" "t" "c")
  ;; data is a list of lists of data - the same length as the format list
  ;; returns the id of the (hidden) primitive  
  (define (build topology indices data)
    (let ((p (build-polygons (length (car (cdr (car data)))) topology)))      
      (letrec 
          ((vec2->vec3 
            (lambda (v)
              (vector (vector-ref v 0) (vector-ref v 1) 0)))
          (set-list
            (lambda (n l name)
              (cond
                ((null? l) 0)
                (else
                 (if (eq? (vector-length (car l)) 2)
                     (pdata-set! name n (vec2->vec3 (car l)))                     
                     (pdata-set! name n (car l)))
                 (set-list (+ n 1) (cdr l) name)))))
           (loop 
            (lambda (data)
              (cond 
                ((null? data) 0)
                (else
                 (set-list 0 (car (cdr (car data))) (car (car data)))
                 (loop (cdr data)))))))
        ;(display data)(newline) 
        (with-primitive p
                        (hide 1)
                        (loop data)
                        (poly-set-index indices))
        )
      p))
  
  
  ;; returns the id of an instance of a pre-built primitive 
  ;; could be build-instance or build-copy...  
  (define (instance-geometry id t r)
    ;(display id)(newline)
    (let ((p (build-copy id)))
      (with-primitive p 
                      (hide 0)
                      (translate t)
                      (rotate r))                      
      p))    
  
  (define (instance-locator t r)
    (let ((p (build-locator)))
      (with-primitive p 
                      (hide 0)
                      (hint-origin)
                      (translate t)
                      (rotate r))                      
      p))
  #|
  
  (define (build topology indices data)
    (display data)(newline) )
  (define (instance-geometry id t r)
    0)      
  (define (instance-locator t r)
    0)
  |#
  ;;--------------------------------------------------------
  ;; parsing geometry
  
  ;; convert a string to a list, using whitespace delimiter
  (define (split s)
    (letrec ((splitter 
              (lambda (sl cur out)
                (cond
                  ((null? sl) (cons (string->number (list->string cur)) out))
                  ((char-whitespace? (car sl))
                   (splitter (cdr sl) '() (cons (string->number (list->string cur)) out)))
                  (else
                   (splitter (cdr sl) (cons (car sl) cur) out))))))
      (splitter (reverse (string->list s)) '() '())))         
   
  ;; returns a float list of data items from an element
  (define (data x) 
    (split (car (get-data x))))
  
  ;; un-interleave the list into a new one
  (define (extract x offset stride)
    (letrec ((loop
              (lambda (x n out)
                (cond 
                  ((null? x) out)
                  ((eq? offset (modulo n stride)) (loop (cdr x) (+ n 1) (cons (car x) out)))
                  (else
                   (loop (cdr x) (+ n 1) out))))))
      (reverse (loop x 0 '()))))
  
  ;; make a list of normals, where the elements refer to the vertices in the same way as the positions
  (define (flatten-vertex-data size position-indices src-indices data)
    (if (not (eq? (length position-indices) (length src-indices))) 
        (error "fluxus-collada-import: model vertex indices size does not match other data"))
    (let ((v (make-vector size 0))    ;; use vectors for
          (n (list->vector data))) ;; random access      
      (letrec ((loop 
                (lambda (i pos-i data-i)
                  (cond
                    ((null? pos-i) 0)
                    (else
                     (vector-set! v (car pos-i) (vector-ref n (car data-i)))
                     (loop (+ i 1) (cdr pos-i) (cdr data-i)))))))
        (loop 0 position-indices src-indices)
        (vector->list v))))
  
  (define (format-floats floats stride)
    (define (format l cur count out)
      (cond
        ((null? l) (cons (list->vector (reverse cur)) out))         
        ((eq? count stride)
         (format (cdr l) (list (car l)) 1 (cons (list->vector (reverse cur)) out)))
        (else
         (format (cdr l) (cons (car l) cur) (+ count 1) out))))
    (reverse (format floats '() 0 '())))
    
  ;; convert float array string directly into a list of vectors
  (define (get-formatted-data x stride)     
    (format-floats (split (car (get-data (find-element x 'float_array)))) stride))
  
  ;; build a list of all the sources mapping names to formatted vector lists
  (define (parse-sources x)
    (define (parse x out)
      (cond 
        ((null? x) out)
        ((eq? (get-name (car x)) 'source)
         (parse (cdr x) (cons (list (get-attribute (car x) 'id) 
                                    (get-formatted-data (car x) 
                                                        (string->number (get-attribute  ; get the stride so we can format 
                                                                         (find-element  ; the data into vectors of the correct size
                                                                          (find-element (car x) 'technique_common) 
                                                                          'accessor) 
                                                                         'stride))))
                                    out)))
        (else
         (parse (cdr x) out))))
    (parse x '()))
  
  ;; lookup source data
  (define (find-source x name)
    (cond
      ((null? x) (error "fluxus-collada-import: can't find a source called" name))
      ((string=? (string-append "#" (car (car x))) name)
       (car (cdr (car x))))
      (else
       (find-source (cdr x) name))))

  ;; loop through all supported vertex data types
  (define (assemble-data x semantic-list fluxus-names sources raw-indices position-indices positions)
    (define (loop semantic-list fluxus-names data)
      (cond 
        ((null? semantic-list) data)
        (else
         (let ((elements (match-elements x 'input 'semantic (car semantic-list))))
           (cond 
             ((not (null? elements))
              (let* ((indices (extract raw-indices (string->number (get-attribute (car elements) 'offset)) (length sources)))                 
                     (vertex-data (flatten-vertex-data (length positions) position-indices indices 
                                                       (find-source sources (get-attribute (car elements) 'source)))))
                (loop (cdr semantic-list) (cdr fluxus-names) (cons (list (car fluxus-names) vertex-data) data))))
             (else
              (loop (cdr semantic-list) (cdr fluxus-names) data)))))))
    (loop semantic-list fluxus-names '()))                          
  
  ;; deal with triangles (todo: generalise for all types)
  (define (parse-triangles x vertex-id sources)        
    (let* ((vertex-elements (match-elements x 'input 'semantic "VERTEX"))
           (raw-indices (data (find-element x 'p)))
           (position-indices (extract raw-indices (string->number (get-attribute (car vertex-elements) 'offset)) (length sources)))
           (positions (find-source sources vertex-id)))
      
      (build 'triangle-list position-indices
             (cons (list "p" positions)
                   (assemble-data x (list "NORMAL" "TEXCOORD") (list "n" "t") sources raw-indices position-indices positions)))))
          
  (define (parse-mesh x)
    ; todo: make the primitive and set the positions
    ; just ignore the technique... ?
    (cond 
      ((element-exists? x 'triangles) ; only cope with triangles atm
       (parse-triangles
        (find-element x 'triangles)
        (get-attribute (find-element (find-element x 'vertices) 'input) 'source)
        (parse-sources (get-elements x))))
      (else
       (error "fluxus-collada-import: no supported primitive type" x))))
  
  (define (parse-geometry x)
    (cond 
      ((element-exists? x 'mesh) ; only cope with mesh
       (parse-mesh (find-element x 'mesh)))
      (else
       (printf "fluxus-collada-import: unsupported geometry type~n"))))
  
  
  (define (parse-geometries x)
    (map
     (lambda (geometry)
       (list
        (get-attribute geometry 'id)
        (parse-geometry geometry)))
     (get-elements (find-element-temp x 'library_geometries))))
  
  ;;-------------------------------------------------------------------------
  ;; scene parsing
  
  (define (get-geometry url geometries)
    (cond
      ((null? geometries) (error "fluxus-collada-import: can't resolve geometry url: " url))
      ((string=? url (string-append "#" (car (car geometries))))
       (car (cdr (car geometries))))
      (else
       (get-geometry url (cdr geometries)))))
  
  (define (get-geometry-url x)
    (cond 
      ((element-exists? x 'instance_geometry)   
       (get-attribute (find-element x 'instance_geometry) 'url))
      (else 
       #f))) 
  
  (define (get-translate x)
    (if (element-exists? x 'translate)
        (list->vector (data (find-element x 'translate)))
        (vector 0 0 0)))
  
  (define (get-rotate x)
    (vector
     (if (element-match-exists? x 'rotate 'sid "rotateX")
         (car (reverse (data (car (match-elements x 'rotate 'sid "rotateX")))))
         0)
     (if (element-match-exists? x 'rotate 'sid "rotateY")
         (car (reverse (data (car (match-elements x 'rotate 'sid "rotateY")))))
         0)
     (if (element-match-exists? x 'rotate 'sid "rotateZ")
         (car (reverse (data (car (match-elements x 'rotate 'sid "rotateZ")))))
         0)))
  
  (define (parse-node x geometries)
    (if (eq? (get-name x) 'node)
        (let ((geometry-url (get-geometry-url x)))
          (if geometry-url ;; if this is a geometry node
              (instance-geometry
               (get-geometry geometry-url geometries) 
               (get-translate x)
               (get-rotate x))
              (instance-locator ;; todo - add lights, cameras
               (get-translate x)
               (get-rotate x))))))
  
  (define (parse-nodes x geometries)
    (map
     (lambda (node)
       (list
        (get-attribute node 'id)
        (parse-node node geometries)))
     (get-elements x)))
  
  
  (define (parse-scenes x geometries)
    (map
     (lambda (scene)
       (list
        (get-attribute scene 'id)
        (parse-nodes scene geometries)))
     (get-elements (find-element-temp x 'library_visual_scenes))))
  
  
  (define (parse x)
    (parse-scenes x (parse-geometries x)))
  
  (define (collada-load filename)
    (let* ((p (open-input-file filename))
           (doc (xml->xexpr (document-element (read-xml p)))))
      ; (display doc)(newline)
      (let ((ret (parse (element-list (cdddr doc)))))
        (close-input-port p)
        ret)))
  
  )

;(require fluxus-collada-import)
;(display (collada-load "/home/dave/Desktop/collada/COLLLADA 1.4.1 Basic Samples/Cube/cube_triangulate.dae"))(newline)

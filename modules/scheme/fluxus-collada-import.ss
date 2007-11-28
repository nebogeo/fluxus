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
  (require (lib "list.ss"))
  (provide
   collada-load
   )
  
  ;;--------------------------------------------------------------------
  ;; xml parsing nonsense...
  
  ;; recursively returns a list of subelements of element x
  ;; deals with whitespace, and the result is usable in the
  ;; following functions
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
  
  ;; accessor for members of element-list
  (define (get-data e)
    (car (cdr (cdr (cdr e)))))
  
  ;; accessor for members of element-list
  (define (get-elements e)
    (car (cdr (cdr e))))
  
  ;; accessor for members of element-list
  (define (get-attributes e)
    (car (cdr e)))
  
  ;; accessor for members of element-list
  (define (get-name e)
    (car e))
  
  ;; does an element exist with this name?
  (define (element-exists? x name)
    (define (find x)
      (cond
        ((null? x) #f)
        ((and (not (string? (car x)))
              (eq? (get-name (car x)) name))
         #t)
        (else
         (find (cdr x)))))
    (find (get-elements x)))
  
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
                       ((null? x) (error "fluxus-collada-import: couldn't find element called " name))
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
  
  ;; look for elements with name 
  (define (match-elements x name)
    (define (loop x out)
      (cond
        ((null? x) out)
        ((eq? (get-name (car x)) name)
         (loop (cdr x) (cons (car x) out)))
        (else
         (loop (cdr x) out))))
    (loop (get-elements x) '()))
  
  ;; look for elements with name and attribute token/value match
  (define (match-elements-attr x name attribute-name attribute-value)
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
  
  (define (element-attr-match-exists? x name attribute-name attribute-value)
    (not (null? (match-elements-attr x name attribute-name attribute-value))))
  
  ;; use a list of element names to drill down like a path
  (define (get-element-from-path x path)
    (cond 
      ((null? path) x)
      (else
       (get-element-from-path (find-element x (car path)) (cdr path)))))
  
  ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Utilities for restructuring model data
  
  ;; look for a key in a simple set
  (define (set-find set key)
    (cond
      ((null? set) #f)
      ((equal? (car set) key) #t)
      (else (set-find (cdr set) key))))
  
  ;; groups the flat index list into multiple lists
  ;; of position/normal/tex coord etc indices
  (define (group-indices indices stride)
    (define (loop indices size count current group)
      (cond 
        ((null? indices) (cons (reverse current) group))
        ((eq? count size) (loop (cdr indices) size 1 (list (car indices)) (cons (reverse current) group)))
        (else (loop (cdr indices) size (+ count 1) (cons (car indices) current) group))))
    (reverse (loop indices stride 0 '() '())))
  
  ;; removes sets of indices which are duplicated
  (define (get-unique-indices indices set)
    (foldl
     (lambda (index set)
       (if (not (member index set))
           (cons index set)
           set))
     '()
     (reverse indices)))
  
  ;; calculates the new indices from the unique lists of multiple indices
  (define (get-new-indices original-indices unique-indices)
    (map
     (lambda (index-list)
       (- (length unique-indices) (length (member index-list unique-indices))))
     original-indices))
  
  ;; reorder vertex data to fit with the new indices (duplicates where required)
  (define (reorder-vertex-data indices data)
    (define (loop-vertex-data vertex-data n dst)
      (cond 
        ((null? vertex-data) dst)
        (else
         (let ((vertex-data-list (car (cdr (car vertex-data)))))
           (loop-vertex-data (cdr vertex-data) (+ n 1) 
                             (cons (cons (car (car vertex-data)) ; stick the fluxus name back on
                                         (list
                                          (map
                                           (lambda (subindices)
                                             (let ((data-index (list-ref subindices n))) ; the actual index for this data type
                                               (cond 
                                                 ((>= data-index (length vertex-data-list))
                                                  (printf "index too large: ~a from ~a~n" data-index subindices)
                                                  (list-ref (car (cdr (car vertex-data))) 0))
                                                 (else                                     
                                                  (list-ref vertex-data-list data-index))))) ; (need to get the list from ("p" positions) etc)
                                           indices)))
                                   dst))))))
    (reverse (loop-vertex-data data 0 '())))
  
  ;; collapses the multiple indices per vertex type into one index shared
  ;; between position/normals/texture coord etc etc. fluxus uses this, as it
  ;; is closer to the underlying interface
  (define (unify-indices/data indices data)
    (let* ((grouped (group-indices indices (length data)))
           (unique-indices (get-unique-indices grouped '()))
           (new-indices (get-new-indices grouped unique-indices)))
      (list new-indices (reorder-vertex-data unique-indices data))))
  
  ;; collapses the multiple indices per vertex type into one index shared
  ;; between position/normals/texture coord etc etc. fluxus uses this, as it
  ;; is closer to the underlying interface
  (define (unify-vertex-data vd)
    (if (not (null? vd))
        (let* ((indices (vertex-data-get-indices vd))
               (data (vertex-data-get-data vd))
               (grouped (group-indices indices (length data)))
           (unique-indices (get-unique-indices grouped '()))
           (new-indices (get-new-indices grouped unique-indices)))
      (list new-indices (reorder-vertex-data unique-indices data)))
        '()))
  
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
  (define (assemble-data x semantic-list fluxus-names sources raw-indices)
    (define (loop semantic-list fluxus-names data)
      (cond 
        ((null? semantic-list) data)
        (else
         (let ((elements (match-elements-attr x 'input 'semantic (car semantic-list))))
           (cond 
             ((not (null? elements))
              (let* ((vertex-data (find-source sources (get-attribute (car elements) 'source))))
                (loop (cdr semantic-list) (cdr fluxus-names) (cons (list (car fluxus-names) vertex-data) data))))
             (else
              (loop (cdr semantic-list) (cdr fluxus-names) data)))))))
    (reverse (loop semantic-list fluxus-names '())))
  
  ;; make vertex data from sources
  (define (parse-vertex-data x vertex-id sources)        
    (let* ((indices (data (find-element x 'p)))
           (positions (find-source sources vertex-id)))      
      (cons indices 
            (list (cons (list "p" positions) (assemble-data x (list "NORMAL" "TEXCOORD") (list "n" "t") sources indices))))))
  
  ;; get indices out of vertex data
  (define (vertex-data-get-indices vd)
    (car vd))

  ;; get the data list from vertex data
  (define (vertex-data-get-data vd)
    (car (cdr vd)))

  ;; join vertex data together
  (define (join-vertex-data vda vdb)
    (list (append (vertex-data-get-indices vda)
                  (vertex-data-get-indices vdb))
          (vertex-data-get-data vda)))
  ; don't seem to need to append vertex data as well as indices, not sure what would
  ; be expected to happen if the source changed across a triangles element...
         #| (list
           (map
            (lambda (data-lista data-listb)
              (list
               (car data-lista)
               (append (car (cdr data-lista))
                       (car (cdr data-listb)))))
            (vertex-data-get-data vda)
            (vertex-data-get-data vdb)))))|#
  
  ;; build a mesh from vertex data
  (define (parse-mesh x)
    ; only deal with triangles at the moment
   (build 'triangle-list 
          (unify-vertex-data 
           (foldl
            (lambda (triangle-element vd)
              (if (null? vd)
                  (parse-vertex-data
                   triangle-element
                   (get-attribute (find-element (find-element x 'vertices) 'input) 'source)
                   (parse-sources (get-elements x)))           
                  (join-vertex-data vd 
                                    (parse-vertex-data
                                     triangle-element
                                     (get-attribute (find-element (find-element x 'vertices) 'input) 'source)
                                     (parse-sources (get-elements x))))))
            '()
            (match-elements x 'triangles)))))
  
  (define (parse-geometry x)
    (cond 
      ((element-exists? x 'mesh) ; only cope with mesh
       (parse-mesh (find-element x 'mesh)))
      (else
       (printf "fluxus-collada-import: unsupported geometry type~n"))))
  
  (define (parse-geometries x)
    (map
     (lambda (geometry)
       (printf "parsing ~a~n" (get-attribute geometry 'id))
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
     (if (element-attr-match-exists? x 'rotate 'sid "rotateX")
         (car (reverse (data (car (match-elements-attr x 'rotate 'sid "rotateX")))))
         0)
     (if (element-attr-match-exists? x 'rotate 'sid "rotateY")
         (car (reverse (data (car (match-elements-attr x 'rotate 'sid "rotateY")))))
         0)
     (if (element-attr-match-exists? x 'rotate 'sid "rotateZ")
         (car (reverse (data (car (match-elements-attr x 'rotate 'sid "rotateZ")))))
         0)))

  (define (get-scale x)
    (if (element-exists? x 'scale)
        (list->vector (data (find-element x 'scale)))
        (vector 1 1 1)))
  
  (define (parse-node x geometries)
    (with-state
     (translate (get-translate x))
     (rotate (get-rotate x))
     (scale (get-scale x))
     (let* ((geometry-url (get-geometry-url x))
            (id (if geometry-url ;; if this is a geometry node
                       (build-copy (get-geometry geometry-url geometries))
                       (with-state (hint-origin) (build-locator)))))
       (with-state
        (parent id)
        (parse-nodes x geometries))
       id)))

  (define (parse-nodes x geometries)
    (map
     (lambda (node)
       (list
        (get-attribute node 'id)
        (parse-node node geometries)))
     (match-elements x 'node)))
  
  
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
      (let ((ret (parse (element-list (cdddr doc)))))
        (close-input-port p)
        ret)))
  
  ;;--------------------------------------------------------
  ;; building geometry
  ;; this is the fluxus interface
  
  ;; topology is the poly type as a symbol - check it's the same as fluxus type
  ;; format describes the contents of data in terms of pdata type eg: ("p" "n" "t" "c")
  ;; data is a list of lists of data - the same length as the format list
  ;; returns the id of the (hidden) primitive  
  (define (build topology vertex-data)
    (if (null? vertex-data) 0
        (let* ((indices (car vertex-data))
               (data (car (cdr vertex-data)))
               (p (build-polygons (length (car (cdr (car data)))) topology)))
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
            (with-primitive p
                            (hide 1)
                            (loop data)
                            (poly-set-index indices))
            )
          p)))
 
  
  
  #|(define (build topology indices/data)
    (printf "build data=~a~n" indices/data))
 |#
  
 
  
  )

;(require fluxus-collada-import)
;(display (collada-load "/home/dave/Desktop/collada/COLLLADA 1.4.1 Basic Samples/Duck/duck_triangulate.dae"))(newline)
;(display (collada-load "/home/dave/Desktop/collada/COLLLADA 1.4.1 Basic Samples/Cube/cube_triangulate.dae"))(newline)
















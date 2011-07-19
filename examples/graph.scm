;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; a graph drawing script, came from exquisite code performance, 
; adapted for naked on pluto and cleaned up for fluxus distro

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; a node just has a name and a position

(define (make-node name pos)
  (cons name (list pos)))

(define (node-name node)
  (car node))

(define (node-pos node)
  (list-ref (cdr node) 0))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; an edge has the node names it connects from and a to 

(define (make-edge from to)
  (list from to))

(define (edge-from edge)
  (car edge))

(define (edge-to edge)
  (cadr edge))

; an edge is equal without worrying about direction
(define (edge=? a b)
  (or 
   (and (eq? (edge-from a) (edge-from b)) (eq? (edge-to a) (edge-to b)))
   (and (eq? (edge-to a) (edge-from b)) (eq? (edge-from a) (edge-to b)))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; basic graph functions

(define (make-graph nodes edges)
  (list nodes edges))

(define (graph-nodes graph)
  (car graph))

(define (graph-edges graph)
  (cadr graph))

(define (graph-find-node graph name)
  (assq name (graph-nodes graph)))
  
(define (graph-find-edge graph edge)
  (foldl
      (lambda (oedge r)
      (if (edge=? edge oedge) #t r))
    #f
    (graph-edges graph)))
  
(define (graph-find-node-edges graph name)
  (foldl
   (lambda (edge r)
     (if (or (eq? (edge-from edge) name) 
             (eq? (edge-to edge) name))
         (cons edge r) r))
   '()
   (graph-edges graph)))

(define (graph-nodes-connected? graph namea nameb)
  (foldl
   (lambda (edge r)
     (if (or (eq? (edge-from edge) nameb)
             (eq? (edge-to edge) nameb))
         edge r))
   #f
   (graph-find-node-edges graph namea)))

(define (graph-remove-disconnected graph)
  (make-graph
   (filter 
    (lambda (node)
      (not (null? (graph-find-node-edges 
                   graph (node-name node)))))
    (graph-nodes graph))
   (graph-edges graph)))

; (cap-list '(1 2 3 4) 2) => '(1 2)
(define (cap-list l c)
  (cond
   ((null? l) '())
   ((zero? c) '())
   (else 
    (cons (car l) (cap-list (cdr l) (- c 1))))))

(define (graph-cap-edges graph cap)
  (graph-remove-disconnected
   (make-graph
    (graph-nodes graph)
    (cap-list (graph-edges graph)))))

; thing involving lists of edges and graphs

; add a list of edges to the graph
(define (add-edges graph edges)
  (foldl
   (lambda (edge graph)
     (add-edge graph edge))
   graph
   edges))

; is this edge in the list of edges?
(define (edges-contains? edges edge)
  (foldl
   (lambda (e r)
     (if (and (not r) (edge=? e edge))
         #t r))
     #f
     edges))

; remove any edges from the graph not in the list
(define (filter-edges graph edges)
  (make-graph
   (graph-nodes graph)
   (filter
    (lambda (edge)
      (edges-contains? edges edge))
    (graph-edges graph))))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; a geoedge is an edge with a length

(define (make-geoedge from to length)
  (list from to length))

(define (geoedge-length edge)
  (list-ref edge 2))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; functions to balance the graph using a spring model

; get the attraction/repulsion value for two connected nodes
(define (balance-connected dst edge-len)
  (if (< dst edge-len) 
    (* 0.01 dst)
    (* -0.01 (- dst edge-len)))) ; make proportional

; get the repulsion value for two disconnected nodes
(define (balance-unconnected dst)
  (* 0.1 (/ 1 dst)))

; change the positions for this node, by comparing it with all the others
(define (geonode-balance node graph)
  (make-node
   (node-name node)
   (foldl
    (lambda (other-node r)
      (let ((name (node-name node))
            (other-name (node-name other-node)))
        (if (not (eq? name other-name))
              (let* ((v (vsub (node-pos node) (node-pos other-node)))
                     (dst (vmag v))
                     (edge (graph-nodes-connected? graph name other-name)))                    
                (vadd r (vmul (vnormalise v) 
                              (if edge  
                                  (balance-connected dst (geoedge-length edge))
                                  (balance-unconnected dst))))) r)))
    (vadd (node-pos node) (vmul (node-pos node) -0.01))
    (graph-nodes graph))))

; change the positions for each node
(define (geograph-balance graph)
  (make-graph
   (map
    (lambda (node)
      (geonode-balance node graph))
    (graph-nodes graph))
   (graph-edges graph)))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; a flxgraph is a fluxus visualisation of a graph

(define (flxgraph nodes edges)
    (list nodes edges))

(define (flxgraph-nodes flxgraph)
    (car flxgraph))

(define (flxgraph-edges flxgraph)
    (cadr flxgraph))

(define (flxgraph-find-node flxgraph name)
    (foldl
        (lambda (node r)
            (if (eq? (car node) name)
                node r))
        #f
        (flxgraph-nodes flxgraph)))

(define (flxgraph-find-edge flxgraph edge)
    (foldl
        (lambda (nedge r)
            (if (edge=? (car nedge) edge)
                nedge r))
        #f
        (flxgraph-edges flxgraph)))

(define (flxgraph-clean flxgraph graph)
    ;    (display (length (flxgraph-nodes flxgraph)))(newline)
    (list 
        (filter
            (lambda (n)
                (cond ((not (graph-find-node graph (car n)))
                        (destroy (cadr n))
                        #f) (else #t)))
            (flxgraph-nodes flxgraph))
        (filter
            (lambda (n)
                (cond ((not (graph-find-edge graph (car n)))
                        (destroy (cadr n))
                        #f) (else #t)))
            (flxgraph-edges flxgraph))))


(define (flxgraph-update flxgraph graph)
  (let ((flxgraph (flxgraph-clean flxgraph graph)))
    (list
     (map
      (lambda (node)
        (let ((n (flxgraph-find-node flxgraph (node-name node))))
          (if n 
              (with-primitive 
               (cadr n)
               (identity)
               (translate (node-pos node))
               (scale 0.25)
               n)
              (with-state
               (hint-unlit)
               (backfacecull 0)
               (colour 0)
               (scale 0.25)
               (list (node-name node)
                     (build-type 
                      "Bitstream-Vera-Sans-Mono.ttf" 
                      (symbol->string (node-name node))))))))
      (graph-nodes graph))
     (map
      (lambda (edge)
        (let ((e (flxgraph-find-edge flxgraph edge)))
          (if e 
              (with-primitive
               (cadr e)
               (pdata-set! "p" 0 (node-pos (graph-find-node graph (edge-from edge))))
               (pdata-set! "p" 1 (node-pos (graph-find-node graph (edge-to edge)))) e)
              (let ((p (with-state
                        (hint-none)
                        (hint-unlit)
                        (hint-wire)
                        (line-width 2)
                        (wire-colour (vector 0 0 0))
                        (build-ribbon 2))))
                (with-primitive p
                                (pdata-set! "p" 0 (node-pos (graph-find-node graph (edge-from edge))))
                                (pdata-set! "p" 1 (node-pos (graph-find-node graph (edge-to edge)))) 
                                (list edge p))))))
      (graph-edges graph)))))



;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; text processing stuff 

(define (string-split s c)
  (define (_ sl tl cl)
    (cond 
      ((null? sl) (if (null? cl) tl (append tl (list (list->string cl)))))
      ((eq? (car sl) c)
       (_ (cdr sl) (append tl (list (list->string cl))) '()))
      (else
       (_ (cdr sl) tl (append cl (list (car sl)))))))
  (_ (string->list s) '() '()))

(define (remove-quotes str)
  (if (eq? #\" (string-ref str 0))
      (substring str 1 (- (string-length str) 1))
      str))

(define (choose l)
  (list-ref l (random (length l))))

(define (read-string-from-file s f)
  (let ((c (read-char f)))
    (if (eof-object? c)
        s
        (read-string-from-file (string-append s (string c)) f))))

(define (file->list filename)
  (let ((f (open-input-file filename)))
    (let ((r (map 
              (lambda (line)
                (list line)) ;(string-split line #\ ))
              (string-split (read-string-from-file "" f) #\newline))))
      (close-input-port f) r)))

(define (string-contains? str char)
  (foldl
   (lambda (c r)
     (if (and (not r) (eq? c char))
         #t r))
   #f
   (string->list str)))

(define (line-contains? line char)
  (foldl
   (lambda (word r)
     (if (and (not r) (string-contains? word char))
         #t r))
   #f
   line))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (add-node graph node)
  (make-graph
   (cons node (graph-nodes graph))
   (graph-edges graph)))

; adds and edge if it does not already exist, and adds the 
; nodes specified if they do not already exist in the graph
(define (add-edge graph edge)
  (let ((from (edge-from edge))
        (to (edge-to edge)))
    
    (if (not (graph-find-edge graph edge))
        (begin
          ;(display edge)(newline)
          (let ((graph 
                 (make-graph 
                  (graph-nodes graph)
                  (cons edge (graph-edges graph)))))
                                        ; add the nodes if they are not present

            (let ((from-node (graph-find-node graph from))
                  (to-node (graph-find-node graph to)))
              ; try and place the new nodes close to nodes they
              ; are attached to
              (let ((graph
                     (if (not from-node)
                         (add-node 
                          graph 
                          (make-node 
                           from
                           (if to-node
                               (vadd (node-pos to-node) 
                                     (vmul (vector (crndf) (crndf) 0) 0.1))
                               (vector (crndf) (crndf) 0))))
                         graph)))
                (if (not to-node)
                    (add-node 
                     graph 
                     (make-node 
                      to
                      (if from-node
                          (vadd (node-pos from-node) 
                                (vmul (vector (crndf) (crndf) 0) 0.1))
                          (vector (crndf) (crndf) 0))))
                    graph)))))
          graph)))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; graphviz stuff

; allows filtering of nodes
(define (check-node name)
  #t)

; convert a string containing "->" to an edge
; should probably trim whitespace etc
(define (process-edge edge-str)
  (let ((chopped (string-split edge-str #\-)))
    (list (car chopped)
          (substring (cadr chopped) 1))))

; get a list of edges from a dot file
(define (dot->edges filename)
  (foldl
   (lambda (edge-line edges)
     (let* ((pe (process-edge (car edge-line)))
            (from (remove-quotes (car pe)))
            (to (remove-quotes (cadr pe))))
       (if (and
            (check-node from)
            (check-node to))
           (cons  
            (make-geoedge
             (string->symbol from)
             (string->symbol to)
             1) edges)
           edges)))
   '()
   (reverse 
    (filter
     (lambda (line)
       (line-contains? line #\>))
     (file->list filename)))))
    
(define (load-dot filename)
  (add-edges 
   (make-graph '() '())
   (dot->edges filename)))
  
; will merge a dot file onto another graph, removing nodes
; and edges not present, and adding new ones as required
; the cap argument allows the number of edges to be controlled
(define (apply-dot graph filename cap)
  (let ((edges (cap-list (dot->edges filename) cap)))
    (graph-remove-disconnected
     (filter-edges 
      (add-edges graph edges)
      edges))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define g (load-dot "graph.dot"))

(clear)
(clear-colour 1)
(define n (flxgraph '() '()))
(set-camera-transform (mtranslate (vector 0 0 -30)))
(define frame 0)

(define (animate)
  ; increase the number of edges with the frame count to
  ; gradually introduce each edge (makes it more pretty, and
  ; resolve to better graphs)
  (set! g (geograph-balance 
           (apply-dot g "graph.dot" 
                      (inexact->exact (truncate (/ frame 5))))))
  (set! n (flxgraph-update n g))
  (set! frame (+ 1 frame)))

(every-frame (animate))



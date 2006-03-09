
; some constants
(define vertex-position 0)
(define vertex-normal 1)
(define vertex-texture 2)

;---------------------------------------------------------------------------------------------------
; tokenise the imput file, dealing with comments, spaces, newlines etc, and only keep what we need

; reads the input file and returns a list of lines, which are lists of words, which in turn are lists of characters
(define tokenise 
    (lambda (fd all currentline currentword)
        (let ((o (read-char fd)))        
            (cond 
                ((eof-object? o)                                                    ; end of file?                           
                    all)                                                            ; finished                               

                ((char=? o #\ )                                                     ; a space?                               
                    (cond ((not (eq? currentword '()))                              ; if the current word is not empty       
                        (set! currentline (append currentline (list currentword)))  ; add the word to the current line       
                        (set! currentword '())))                                    ; reset the word                       
                    (tokenise fd all currentline currentword))                      ; continue                               

                ((char=? o #\newline)                                               ; a newline?                           
                    (cond ((not (eq? currentword '()))                              ; if the current word is not empty       
                        (set! currentline (append currentline (list currentword)))  ; add the word to the current line       
                        (set! currentword '())))                                    ; reset the word                       
                    (cond ((not (eq? currentline '()))                              ; if the current line is not empty       
                        (set! all (append all (list currentline)))                  ; add the line to the complete list    
                        (set! currentline '())))                                    ; reset the current line               
                    (tokenise fd all currentline currentword))                      ; continue                               

                ((set! currentword (append currentword (list o)))                   ; add the char to the current word       
                (tokenise fd all currentline currentword))))))

;---------------------------------------------------------------------------------------------------
; extract all the data from the vertices, textures and normals and convert into scheme vectors

; just converts lists of chars into floating point numbers
(define parse-number
    (lambda (input)
        (eval-string (list->string input))))

; converts three lists of chars into a vector
(define parse-vector
    (lambda (input)
        (vector (parse-number (list-ref input 0))
                (parse-number (list-ref input 1))
                (parse-number (list-ref input 2)))))

; extracts a vector from the input list
(define parse-vertex
    (lambda (input object-data)
        (append object-data (list (parse-vector input)))))   ; just add the vector

; looks at the first token on the line and decides which function to call
(define parse-line
    (lambda (input object-data)
        (let ((token (list->string (car input))))     ; first word on the line
            (cond
                ((string=? token "v")               ; vertex positions
                    (list-set! object-data vertex-position     ; set the vertex positions list to the result of 
                        (parse-vertex (cdr input) (list-ref object-data vertex-position))))) ; parse vertex (which also takes the list as input)          
        object-data)))
        
; top level parser, just calls parse-line for every line
(define parse 
    (lambda (input object-data)
        (set! object-data (parse-line (car input) object-data))
        (if (eq? (cdr input) '())
            object-data
            (parse (cdr input) object-data))))
    
;---------------------------------------------------------------------------------------------------
; take all the object data, loop through the faces putting them together - dereferencing the 
; vertex indices into a flat list

; gets the value from between the '/'es - ie 1/2/3
(define extract-index 
    (lambda (input pos output)
        (cond
            ((eq? (car input) #\/)          ; if we have a divider
                (set! pos (- pos 1))
                (if (> pos 0)                ; we still have more to go
                    (set! output '())))     ; clear the output        
            ((set! output (append output (list (car input))))))
        (if (or (< pos 1) (eq? (cdr input) '()))
            output
            (extract-index (cdr input) pos output))))

(define parse-face-vertex
    (lambda (input object-data built-vertices)
        (append built-vertices (list           ; add a list containing
            (list-ref 
                (list-ref object-data vertex-position)  ; our position list, and index by...
                (- (eval-string (list->string (extract-index input 1 '()))) 1)))))) ; the vertex position index
            

(define parse-face 
    (lambda (input object-data built-vertices)
        (set! built-vertices (parse-face-vertex (car input) object-data built-vertices))
        (if (eq? (cdr input) '())        
            built-vertices
            (parse-face (cdr input) object-data built-vertices))))

(define build 
    (lambda (input object-data built-vertices)
        (let ((token (list->string (car (car input)))))   ; first word on the line
        (cond
            ((string=? token "f")                          ; face
                    (let ((face '()))
                    (set! face (parse-face (cdr (car input)) object-data face))
                    (set! built-vertices (append built-vertices (list face)))))))
        (if (eq? (cdr input) '())
            built-vertices
            (build (cdr input) object-data built-vertices))))
    
;---------------------------------------------------------------------------------------------------

(define obj-load 
    (lambda (filename)
        ; object data list consists of:
        ; ((list of vectors) (list of normals) (list of texture coordinates) (...))
        (define obdata '(() () ()))

        (define tokens (tokenise (open-input-file filename) '() '() '()))
        (build tokens (parse tokens obdata) '())))
    
;---------------------------------------------------------------------------------------------------
; fluxus part - convert this data into a primitive

; loops through a face, setting the vertices
(define make-face-vertices
    (lambda (vertices n)
        (pdata-set "p" n (car vertices)) ; set the vert position
        (set! n (+ n 1))    ; this is the global index, which we also have to return
        (if (eq? (cdr vertices) '())
            n
            (make-face-vertices (cdr vertices) n))))
 
; loop through the faces
(define make-vertices
    (lambda (vertices n)
        (if (eq? (length (car vertices)) 3) ; only for triangular faces for the moment
                (set! n (make-face-vertices (car vertices) n)))
        (if (eq? (cdr vertices) '())
            0
            (make-vertices (cdr vertices) n))))

; builds a new primitive and sets it up with the supplied data
(define obj-make 
    (lambda (vertices)
        (let ((shape (build-polygons (* (length vertices) 3) 2))) ; make our primitive
        (grab shape)
        (make-vertices vertices 0) ; set the data
        (recalc-normals 1)           
        (ungrab)
		shape)))

; print out the vert data
(define dump 
    (lambda (vertices)
        (display (car vertices))(newline)
        (if (eq? (cdr vertices) '())
            0
            (dump (cdr vertices)))))

;---------------------------------------------------------------------------------------------------


;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; auto documentation generator for fluxus
;; 
;; parses C++ files looking for function documentation
;; and then converts this into a scheme helpmap list 

(module autodoc mzscheme
  (provide autodoc)
  
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
    (string=? (read-word file) "StartFunctionDoc"))
  
  ; gather all words up to end word
  (define (gather file end)
    (define (inner-gather str)
      (let ((w (read-word file)))
        (cond 
          ((string=? w end)
           str)
          ((string=? w "//") ; ignore comments 
           (inner-gather str))
          ((string=? str "")
           (inner-gather (string-append w)))
          (else
           (inner-gather (string-append str " " w))))))
    (inner-gather ""))
  
  ; gather all words up to end word including newlines
  (define (gather-formatted file end)
    (define (inner-gather str)
      (let ((w (read-word-newline file)))
        (cond 
          ((and (>= (string-length w) (string-length end)) ; for when we have a newline 
                (string=? (substring w 0 (string-length end)) end)) ; stuck on the end
           str)
          ((string=? str "")
           (inner-gather (string-append w)))
          (else
           (inner-gather (string-append str " " w))))))
    (define (remove-comments str)
      (define (inner-remove-comments in out)
        (if (null? (cddr in))
            out
            (if (and (char=? (car in) #\/)
                     (char=? (car (cdr in)) #\/))
                (inner-remove-comments (cddr in) out)
                (inner-remove-comments (cdr in) (append out (list (car in)))))))
      (list->string (inner-remove-comments (string->list str) '())))
    
    (remove-comments (inner-gather "")))
  
  ; extracts the data we need from the C++ file and 
  ; adds it to the input list of strings per function
  ; in the form:
  ; ((funcname (arguments description example)))
  
  (define (parse file l)
    (let ((c (peek-char file)))
      (if (eof-object? c)
          l
          (cond 
            ((not (functiondoc? file))
             (parse file l))
            (else
             (read-word file) ; comment //
             (parse file (append l (list (list
                                          (read-word file) ; funcname
                                          (list
                                           (gather file "Description:")
                                           (gather file "Example:")
                                           (gather-formatted file "EndFunctionDoc")))))))))))
  
  
  (define (parse-file inputfilename helpmap)
    (let ((file (open-input-file inputfilename)))
      (let ((t (parse file helpmap)))
        (close-input-port file)
        t)))
  
  (define (parse-files inputfilenames helpmap)
    (if (null? inputfilenames)
        helpmap
        (parse-files (cdr inputfilenames) (parse-file (car inputfilenames) helpmap))))
  
  (define (autodoc helpmapfilename files)
    (let ((helpmapfile (open-output-file helpmapfilename)))
      (write (parse-files files '()) helpmapfile)
      (close-output-port helpmapfile)))
  
  ) ; module autodoc



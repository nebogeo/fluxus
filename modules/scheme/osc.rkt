;; -*- mode: scheme; -*-
;;
;; derived from the osc code in rsc3 (c) rohan drape, 1998-2007 
;; last seen at http://www.slavepianos.org/rd/sw/rsc3.r6rs/osc/
;; with a few related yak haired tangles for fluxus.
;;
;; GPL.2, see http://www.gnu.org/copyleft
;; 
;; authors
;;  - nik gaffney <nik@fo.am>
;;
;; comments
;;  - http://twitter.com/zzkt/status/6090836483
;;  - doesn't yet work with fluxus

(module osc scheme
  
  (require  rnrs/base-6
            rnrs/bytevectors-6
            rnrs/io/ports-6
            (only-in srfi/1 iota find-tail)
            rnrs/records/syntactic-6
            ;           (rsc3 ntp ntp)
            ;           (rsc3 u8 read)
            ;           (rsc3 osc encode)
            ;           (rsc3 osc type)
            ;           (only (rnrs r5rs) modulo)
            ;           (only (rsc3 collection list) iota)
            ;           (only (rsc3 collection list) find-tail)        
            ;           (only (rnrs r5rs) modulo)
            ;           (only (rsc3 collection list) make-list)
            ;           (rsc3 ntp ntp)
            ;           (rsc3 u8 u8t)
            ;           (rsc3 u8 encode)
            ;           (only (rsc3 osc verify) bundle? message?)
            ;           (rnrs bytevectors)
            ;           (ikarus)
            ;           (rsc3 osc decode)
            ;           (rsc3 osc encode)
            ;           (rsc3 u8 encode)
            ;           (rsc3 u8 decode)
            )
  
  
  
  (provide osc-display
           decode-osc
           oI32 oI64 oU64 oF32 oF64 oStr oByt
           message message? bundle bundle?
           cstring-length encode-osc encode-bundle-ntp osc->u8t
           open-tcp close-transport
           osc-send osc-recv osc-request
           purify)
  
  ;; misc
  
  (define nil (list))
  
  
  ;; number a => (bytevector -> int -> a -> ()) -> int -> a
  (define bytevector-make-and-set1
    (lambda (f k n)
      (let ((v (make-bytevector k 0)))
        (f v 0 n)
        v)))
  
    ;; port -> string
  (define read-cstr
    (lambda (p)
      (let loop ((l nil)
                 (b (get-u8 p)))
        (if (= b 0)
            (list->string (map integer->char (reverse l)))
            (loop (cons b l) (get-u8 p))))))
  
    ;; port -> int -> bytevector
  (define read-bstr
    (lambda (p n)
      (get-bytevector-n p n)))
  
    ;; port -> int
  (define read-i32
    (lambda (p)
      (decode-i32 (read-bstr p 4))))
    
    ;; bytevector -> int
  (define decode-u8
    (lambda (v)
      (bytevector-u8-ref v 0)))
  
  ;; bytevector -> int
  (define decode-u16
    (lambda (v)
      (bytevector-u16-ref v 0 (endianness big))))
  
  ;; bytevector -> int
  (define decode-u32
    (lambda (v)
      (bytevector-u32-ref v 0 (endianness big))))
  
  ;; bytevector -> int
  (define decode-u64
    (lambda (v)
      (bytevector-u64-ref v 0 (endianness big))))
  
  ;; bytevector -> int
  (define decode-i8
    (lambda (v)
      (bytevector-s8-ref v 0)))
  
  ;; bytevector -> int
  (define decode-i16
    (lambda (v)
      (bytevector-s16-ref v 0 (endianness big))))
  
  ;; bytevector -> int
  (define decode-i32
    (lambda (v)
      (bytevector-s32-ref v 0 (endianness big))))
  
  ;; bytevector -> int
  (define decode-i64
    (lambda (v)
      (bytevector-s64-ref v 0 (endianness big))))
  
  ;; bytevector -> double
  (define decode-f32
    (lambda (v)
      (bytevector-ieee-single-ref v 0 (endianness big))))
  
  ;; bytevector -> double
  (define decode-f64
    (lambda (v)
      (bytevector-ieee-double-ref v 0 (endianness big))))
  
  ;; bytevector -> string
  (define decode-str
    (lambda (b)
      (utf8->string b)))
  
  ;; bytevector -> string
  (define decode-pstr
    (lambda (v)
      (let* ((n (decode-u8 v))
             (w (bytevector-section v 1 (+ n 1))))
        (decode-str w))))
  
  ;; bytevector -> string
  (define decode-cstr
    (lambda (v)
      (let* ((n (bytevector-find-index v 0))
             (w (bytevector-section v 0 n)))
        (decode-str w))))
  
  ;; int -> bytevector
  (define encode-u8
    (lambda (n)
      (bytevector-make-and-set1
       bytevector-u8-set!
       1
       (exact n))))
  
  ;; int -> bytevector
  (define encode-u16
    (lambda (n)
      (bytevector-make-and-set
       bytevector-u16-set!
       2
       (exact n))))
  
  ;; int -> bytevector
  (define encode-u32
    (lambda (n)
      (bytevector-make-and-set
       bytevector-u32-set!
       4
       (exact n))))
  
  ;; int -> bytevector
  (define encode-u64
    (lambda (n)
      (bytevector-make-and-set
       bytevector-u64-set!
       8
       (exact n))))
  
  ;; int -> bytevector
  (define encode-i8
    (lambda (n)
      (bytevector-make-and-set1
       bytevector-s8-set!
       1
       (exact n))))
  
  ;; int -> bytevector
  (define encode-i16
    (lambda (n)
      (bytevector-make-and-set
       bytevector-s16-set!
       2
       (exact n))))
  
  ;; int -> bytevector
  (define encode-i32
    (lambda (n)
      (bytevector-make-and-set
       bytevector-s32-set!
       4
       (exact n))))
  
  ;; int -> bytevector
  (define encode-i64
    (lambda (n)
      (bytevector-make-and-set
       bytevector-s64-set!
       8
       (exact n))))
  
  ;; double -> bytevector
  (define encode-f32
    (lambda (n)
      (bytevector-make-and-set
       bytevector-ieee-single-set!
       4
       (inexact n))))
  
  ;; double -> bytevector
  (define encode-f64
    (lambda (n)
      (bytevector-make-and-set
       bytevector-ieee-double-set!
       8
       (inexact n))))
  
  ;; string -> bytevector
  (define encode-str
    (lambda (s)
      (string->utf8 s)))
  
  ;; string -> bytevector
  (define encode-pstr
    (lambda (s)
      (let* ((b (encode-str s))
             (n (encode-u8 (bytevector-length b))))
        (list n b))))
  
  ;; string -> bytevector
  (define encode-cstr
    (lambda (s)
      (let* ((b (encode-str s))
             (z (encode-u8 0)))
        (list b z))))

  
    ;; port -> string
  (define read-pstr
    (lambda (p)
      (let* ((n (lookahead-u8 p))
             (v (read-bstr p (+ n 1))))
        (decode-pstr v))))
    
  
  ;; port -> int
  (define read-i16
    (lambda (p)
      (decode-i16 (read-bstr p 2))))
  
  ;; port -> int
  (define read-u16
    (lambda (p)
      (decode-u16 (read-bstr p 2))))
  
  
  ;; port -> int
  (define read-u32
    (lambda (p)
      (decode-u32 (read-bstr p 4))))
  
  ;; port -> int
  (define read-i64
    (lambda (p)
      (decode-i64 (read-bstr p 8))))
  
  ;; port -> int
  (define read-u64
    (lambda (p)
      (decode-u64 (read-bstr p 8))))
  
  ;; port -> double
  (define read-f32
    (lambda (p)
      (decode-f32 (read-bstr p 4))))
  
  ;; port -> double
  (define read-f64
    (lambda (p)
      (decode-f64 (read-bstr p 8))))
  
  ;; int
  (define seconds-from-1900-to-1970
    (+ (* 70 365 24 60 60) (* 17 24 60 60)))
  
  ;; double -> int
  (define ntpr->ntp
    (lambda (n)
      (exact (round (* n (expt 2 32))))))
  
  ;; double -> double
  (define utc->ntpr
    (lambda (n)
      (+ n seconds-from-1900-to-1970)))
  
  ;; int -> doubl
  (define ntp->utc
    (lambda (n)
      (- (/ n (expt 2 32)) seconds-from-1900-to-1970)))
  
  
  
    ;; bytevector -> int -> int -> bytevector
  (define bytevector-section
    (lambda (v l r)
      (let* ((n (- r l))
             (w (make-bytevector n 0)))
        (bytevector-copy! v l w 0 n)
        w)))
  
    ;; bytevector -> byte -> int
  (define bytevector-find-index
    (lambda (v x)
      (letrec ((f (lambda (i)
                    (if (= (bytevector-u8-ref v i) x)
                        i
                        (f (+ i 1))))))
        (f 0))))

    
  ;; number a => (bytevector -> int -> a -> ()) -> int -> a
  (define bytevector-make-and-set
    (lambda (f k n)
      (let ((v (make-bytevector k 0)))
        (f v 0 n (endianness big))
        v)))
      
    ;; sum :: (Num a) => [a] -> a
  (define sum
    (lambda (l)
      (foldl + 0 l)))
  
  (define (u8t->bytevector t)
  (let* ((l (flatten t))
         (n (map bytevector-length l))
         (m (sum n))
         (v (make-bytevector m)))
    (let loop ((i 0)
               (l l)
               (n n))
      (if (null? l)
          v
          (let ((l0 (car l))
                (n0 (car n)))
            (bytevector-copy! l0 0 v i n0)
            (loop (+ i n0) (cdr l) (cdr n)))))))
  
  (define (padding-of n) (make-list n (encode-u8 0)))
  
  
  
  
  
  
  
  ;; decode.ss
  
  
  ;; OSC strings are C strings padded to a four byte boundary.
  
  (define (read-ostr)
    (let* ((s (read-cstr))
           (n (modulo (cstring-length s) 4))
           (p (- 4 (modulo n 4))))
      (if (not (= n 0))
          (read-bstr p)
          #f)
      s))
  
  ;; OSC byte strings are length prefixed.
  
  (define (read-obyt)
    (let* ((n (read-i32))
           (b (read-bstr n))
           (p (- 4 (modulo n 4))))
      (if (not (= n 0))
          (read-bstr p)
          #f)
      b))
  
  ;; Evaluates to the object, described by the OSC type character
  ;; `type', encoded at the OSC byte stream `p'.
  
  (define (read-value t)
    (cond
      ((eq? t oI32) (read-i32))
      ((eq? t oI64) (read-i64))
      ((eq? t oU64) (read-u64))
      ((eq? t oF32) (read-f32))
      ((eq? t oF64) (read-f64))
      ((eq? t oStr) (read-ostr))
      ((eq? t oByt) (read-obyt))
      (else         (error 'read-value "bad type" t))))
  
  ;; Evaluate to the list of objects encoded at the OSC byte stream
  ;; `p', conforming to the types given in the OSC character type
  ;; list `types'.
  
  (define (read-arguments types)
    (if (null? types)
        '()
        (cons (read-value (car types))
              (read-arguments (cdr types)))))
  
  ;; Evaluate to the scheme representation of the OSC message at the OSC
  ;; byte stream `p'.  The first object is the 'address' of the
  ;; message, any subsequent objects are arguments for that address.
  
  (define (read-message)
    (let* ((address (read-ostr))
           (types (read-ostr)))
      (cons address
            (read-arguments (cdr (string->list types))))))
  
  ;; Evaluate to a scheme representation of the OSC bundle encoded at
  ;; the OSC stream `p'.  The bundle ends at the end of the byte
  ;; stream.  The first object is the <real> UTC 'timetag' of the
  ;; bundle, any subsequent objects are either OSC messages or embedded
  ;; OSC bundles.
  
  (define (read-bundle)
    (let ((bundletag (read-ostr))
          (timetag (ntp->utc (read-u64)))
          (parts (list)))
      (if (not (equal? bundletag "#bundle"))
          (error 'read-bundle "illegal bundle tag" bundletag)
          (cons timetag
                (let loop ((parts (list)))
                  (if (eof-object? (lookahead-u8 (current-input-port)))
                      (reverse parts)
                      (begin
                        ;; We have no use for the message size...
                        (read-i32)
                        (loop (cons (read-packet) parts)))))))))
  
  ;; Evaluate to the scheme representation of the OSC packet encoded at
  ;; the OSC byte stream `p'. An OSC packet is either an OSC message
  ;; or an OSC bundle.
  
  (define hash-u8 (char->integer #\#))
  
  (define (read-packet)
    (if (eq? (lookahead-u8 (current-input-port)) hash-u8)
        (read-bundle)
        (read-message)))
  
  (define (with-input-from-bytevector b f)
    (parameterize
        ((current-input-port (open-bytevector-input-port b)))
      (f)))
  
  (define (decode-osc b)
    (with-input-from-bytevector b read-packet))
  
  
  
  
  ;; display.ss
  
  
  ;; Write a text representation of the OSC u8l `l'.  The format is that
  ;; used throughout the OSC specification.
  
  (define (osc-display l)
    (for-each
     (lambda (b n)
       (display (format "~a (~a)" (number->string b 16) (integer->char b)))
       (if (= 3 (modulo n 4))
           (newline)
           (display #\space)))
     l
     (iota (length l))))
  
  
  ;; encode.ss
  
  ;; OSC strings are C strings padded to a four byte boundary.
  
  (define (cstring-length s)
    (+ 1 (string-length s)))
  
  (define (encode-string s)
    (let ((n (modulo (cstring-length s) 4)))
      (list (encode-cstr s)
            (if (= n 0)
                (list)
                (padding-of (- 4 n))))))
  
  ;; OSC byte strings are length prefixed?
  
  (define (encode-bytes b)
    (let* ((n (bytevector-length b))
           (n* (modulo n 4)))
      (list (encode-i32 n)
            b
            (if (= n* 0)
                (list)
                (padding-of (- 4 n*))))))
  
  ;; Allowable types are <integer>, <real>, <string>, or <u8l>.  Note
  ;; further that determining if a <real> should be written as a float
  ;; or a double is non-trivial and not undertaken here, all <real>s are
  ;; written as floats.
  
  (define (exact-integer? n)
    (and (integer? n) (exact? n)))
  
  (define (encode-value e)
    (cond ((exact-integer? e)   (encode-i32 e))
          ((real? e)            (encode-f32 e))
          ((string? e)          (encode-string e))
          ((bytevector? e)      (encode-bytes e))
          (else                 (error 'encode-value "illegal value" e))))
  
  ;; Encode the type string for the Evaluates to the OSC <u8v>
  ;; indicating the types of the elements of the list `l'.
  
  (define (encode-types l)
    (encode-string
     (list->string
      (cons #\,
            (map (lambda (e)
                   (cond ((exact-integer? e) #\i)
                         ((real? e)          #\f)
                         ((string? e)        #\s)
                         ((bytevector? e)    #\b)
                         (else               (error 'encode-types "type?" e))))
                 l)))))
  
  ;; Encode OSC message.
  
  (define (encode-message m)
    (list (encode-string (car m))
          (encode-types (cdr m))
          (map encode-value (cdr m))))
  
  ;; Encode OSC bundle. The first element is a <real> valued UTC
  ;; 'time-tag', each subsequent element must be an OSC 'message'.
  
  (define (encode-bundle-ntp b)
    (list (encode-string "#bundle")
          (encode-u64 (ntpr->ntp (car b)))
          (map (lambda (e)
                 (if (message? e)
                     (encode-bytes (encode-osc e))
                     (error 'encode-bundle "illegal value" e)))
               (cdr b))))
  
  (define (encode-bundle b)
    (encode-bundle-ntp (cons (utc->ntpr (car b)) (cdr b))))
  
  ;; An OSC packet is either an OSC message or an OSC bundle.
  
  (define (osc->u8t p)
    (if (bundle? p)
        (encode-bundle p)
        (encode-message p)))
  
  (define (encode-osc p)
    (u8t->bytevector (osc->u8t p)))
  
  
  ;; purify.ss
  
  
  ;; Evaluates to a type-correct form of the OSC data `e'.  This
  ;; procedure does not verify that `e' is syntactically correct.
  ;; Boolean values are rewritten as integers, zero for '#f' and one for
  ;; '#t'.  Symbols are rewritten as the strings given by
  ;; 'symbol->string'.  An error is raised if `e' cannot be rewritten.
  ;; Note that R5RS does not require symbols to be case sensitive
  ;; although most interpreters will have an option to set this.
  
  (define (purify e)
    (cond ((or (number? e) (string? e) (bytevector? e)) e)
          ((list? e) (map purify e))
          ((symbol? e) (symbol->string e))
          ((boolean? e) (if e 1 0))
          (else (error 'purify "illegal input" e))))
  
  
  
  
  ;; transport.ss
  
  
  (define-record-type transport (fields type ip op))
  
  (define (tcp-transport? t)
    (eq? (transport-type t) 'tcp))
  
  (define (udp-transport? t)
    (eq? (transport-type t) 'tcp))
  
  (define (open-tcp h p)
    (let-values (((op ip) (tcp-connect h p)))
      (make-transport 'tcp ip op)))
  
  (define (close-transport t)
    (close-input-port (transport-ip t))
    (close-output-port (transport-op t)))
  
  (define (put-bytevector* p v)
    (let ((n (bytevector-length v)))
      (let loop ((i 0))
        (if (= i n)
            #f
            (let ((b (bytevector-u8-ref v i)))
              (put-u8 p b)
              (loop (+ i 1)))))))
  
  (define (osc-send t m)
    (let ((p (transport-op t))
          (v (encode-osc m)))
      (cond ((tcp-transport? t)
             (put-bytevector* p (encode-i32 (bytevector-length v)))
             (put-bytevector* p v))
            (else
             (error 'osc-send "unknown transport")))))
  
  (define (osc-recv t _)
    (error 'osc-recv "unknown transport"))
  
  (define (osc-request u r m t)
    (osc-send u m)
    (let ((p (osc-recv u t)))
      (if (and p (string=? (car p) r)) p #f)))
  
  
  
  ;; type.ss
  
  
  (define oI32 #\i)
  (define oI64 #\h)
  (define oU64 #\t)
  (define oF32 #\f)
  (define oF64 #\d)
  (define oStr #\s)
  (define oByt #\b)
  
  
  ;; verify.ss
  
  ;; Validating constructors.
  
  (define (message c . l)
    (if (string? c)
        (cons c l)
        (error 'message "illegal command" c)))
  
  (define (bundle t . l)
    (if (number? t)
        (cons t l)
        (error 'bundle "illegal timestamp" t)))
  
  ;; Predicates for OSC packet types.
  
  (define (message? p)
    (string? (car p)))
  
  (define (bundle? p)
    (number? (car p)))
  
  ;; Evaluates to '#t' iff `m' is a correct OSC message.  The first
  ;; element must be a string 'address', subsequent elements are
  ;; arguments of types integer, real or string.
  
  (define (verify-message m)
    (and (string? (car m))
         (not (find-tail (lambda (e) (not (or (integer? e)
                                              (real? e)
                                              (string? e))))
                         (cdr m)))))
  
  ;; Evaluates to '#t' iff `b' is a correct OSC bundle.  The first
  ;; element must be an integer 'timetag', subsequent elements may be OSC
  ;; messages or OSC bundles.  The timetags of embedded bundles must be
  ;; greater than or equal to the timetag of the containing bundle.
  
  (define (verify-bundle b)
    (and (integer? (car b))
         (not (find-tail (lambda (e) (not (or (verify-message e)
                                              (and (verify-bundle e)
                                                   (>= (car e) (car b))))))
                         (cdr b)))))
  
  ;; Evaluates to '#t' iff `p' is a correct OSC packet.  An OSC packet
  ;; is either an OSC message or an OSC bundle.
  
  (define (verify-packet p)
    (or (verify-message p)
        (verify-bundle p)))
  
  )

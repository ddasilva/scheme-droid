;;; (emit tokens port) - will format tokens like a Java class.
;;; Tokens can contain nested lists which a flattened.
;;; The tokens ";" "{" { "}" and } are used to control indenting.

;;; For example:

;;; > (emit '(public interface Function1 {
;;;             public Object call "(" Object a ")" ";" })
;;; 	(current-output-port))
;;; public interface Function1 {
;;;   public Object call(Object a);
;;;   }
;;; ()

(define (emit arg port)
  (let ((indent-level 0)
	(last-space #t))		; Last output ended with whitespace.
    (define (emit0 arg)
      (if (pair? arg) (for-each (lambda (a) (emit0 a)) arg)
	  (if (null? arg) ()
	      (wspace arg))))
    (define (wchar char)		; Output a character.
      (write-char char port))
    (define (wnewline step)
      ;; Output newline and indent indent-level + step.
      (set! indent-level (+ indent-level step))
      (write-char #\newline port)
      (do ((i 0 (+ i 1)))
	  ((>= i indent-level))
	(write-char #\space port)
	(write-char #\space port))
      (set! last-space #t))
    (define (wdisplay arg)
      ;; Display arg checking indent and spacing.
      (display arg port)
      (cond
       ((or (eq? arg '{) (equal? arg "{"))
	(wnewline 1))
       ((or (eq? arg '})(equal? arg "}"))
	(wnewline -1))
       ((equal? arg ";") (wnewline 0))
       (else (set! last-space (ends-with-space? arg)))))
    (define (needs-space? arg)
      (and (not last-space)
	   (not (equal? arg "\""))
	   (not (equal? arg "."))
	   (not (equal? arg "("))
	   (not (equal? arg ")"))
	   (not (equal? arg ","))
	   (not (equal? arg ";"))))
    (define (wspace arg)
      (if (needs-space? arg) (wchar #\space))
      (wdisplay arg))
    (emit0 arg)))

;;; Comments only occur at the current indent level.
;;; If the comment is public? "/** ... **/" is used, otherwise "/* ... */"
(define (make-comment public? . body) (vector 'comment type body))
(define (comment? c) (and (vector? c) (eq? (vector-ref c 0) 'comment)))
(define (comment-public? c) (vector-ref c 1))
(define (comment-body c) (vecto-ref c 2))

(define (string-ends-with-space s)
  (let ((c (string-ref s (- (string-length s) 1))))
    (or (eqv? c #\space)
	(eqv? c #\.)
	(eqv? c #\())))

(define (ends-with-space? arg)
  ;; Memoize for symbols?
  (cond 
   ((equal? arg "#") #t)		; Ambiguous case. +++ What case?
   ((equal? arg "(") #t)
   ((equal? arg ".") #t)
   ((number? arg) #f)
   ((symbol? arg) (string-ends-with-space (symbol->string arg)))
   ((string? arg) (string-ends-with-space arg))
   (else (error arg " unexpected type"))))

(define (method-name prefix name)
  ;; Generate an accessor name.
  (let ((s (string-append name)))
    (string->symbol (string-append prefix
				   (char-upcase (string-ref s 0))
				   (.substring s 1)))))

(define (field-declaration type name) `(,type ,name ";"))
(define (set-field name value) `("this." ,name "=" ,value ";"))

(define (getter-method type name)
  `(public ,type ,(method-name 'get name)
    "(" ")" "{"
    return this "." ,name ";"
    "}"))

(define (setter-method type name)
  `(public ,type ,(method-name 'set name)
    "(" ,type ,name "," ,type value ")" "{"
    ,(set-field name 'value)
    "}"))

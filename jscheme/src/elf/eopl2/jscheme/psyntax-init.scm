;;;
;;; Interface jscheme to psyntax.
;;;
(load "elf/util.scm")

(define (void) #null)

(define (andmap f first . rest)
  (or (null? first)
      (if (null? rest)
	  (let andmap ((first first))
	    (let ((x (car first)) (first (cdr first)))
	      (if (null? first)
		  (f x)
		  (and (f x) (andmap first)))))
	  (let andmap ((first first) (rest rest))
	    (let ((x (car first))
		  (xr (map car rest))
		  (first (cdr first))
		  (rest (map cdr rest)))
	      (if (null? first)
		  (apply f (cons x xr))
		  (and (apply f (cons x xr)) (andmap first rest))))))))

(define (ormap proc list1)
  (and (not (null? list1))
       (or (proc (car list1)) (ormap proc (cdr list1)))))

;;; Just use Jscheme's error for now.
'(define error
  ;; Need something better here.
  (let ((original-error error))
    (lambda (who format-string why what)
      (original-error (string-append "Error in " who ": " why " " what".")))))

(define gensym
  (let ((c 0))
    (lambda ()
      (string->symbol (string-append "g" (set! c (+ c 1)))))))

(define property-table (Hashtable.))
(define (getprop symbol key)
  (let ((it (assoc key (cdr (%getprops symbol)))))
    (if it (cdr it))))

(define (%getprops symbol)
  (let ((props (.get property-table symbol)))
    (if (eq? props #null)
	(let ((props (list symbol)))
	  (.put property-table symbol props)
	  props)
	props)))

(define (putprop symbol key value)
    (let* ((props (%getprops symbol))
	   (it (assoc key (cdr props))))
      (if it (let ((old-value (cdr it)))
	       (set-cdr! it value)
	       old-value)
	  (begin 
	    (set-cdr! props (cons (cons key value) (cdr props)))
	    #f))))

(define (remprop symbol key)
  (define (remprop0 props key)
    (if (null? (cdr props)) #f
	(if (eq? (car (car (cdr props))) key)
	    (set-cdr! props (cdr (cdr props)))
	    (remprop0 (cdr props) key))))

  (let ((props (%getprops symbol)))
    (remprop0 props key)))

(define (self-evaluating? x)
  (or (boolean? x) (number? x) (string? x) (char? x) (null? x)
      (eq? x #null)))

(load "elf/eopl2/jscheme/psyntax.pp")


(tryCatch
;; Capture old definition of eval.
 old-eval
 (lambda (e)
   (set! old-eval eval)))

(define (eval x . $ignore)
  ;; Redefine eval to use define-syntax!
  (old-eval (sc-expand x)))


(define (load file)
  ;; Since eval is redefined, redefine load.
  (let ((s (Scheme.open file)))
    (if (isNull s) (error {file [file] not found.})
	(let loop ((it (read s)))
	  (if (eof-object? it) #t
	      (begin
		;; (print it)
		(eval it)
		(loop (read s))))))))

;;; At this point define-syntax rules!

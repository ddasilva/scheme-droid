;;;
;;; Simple testing scaffolding;
;;;
(define-macro (assert p)
  ;; Usage: `(assert ,truthhood)
  ;; errors if ,truthhood is a falsehood.
  `(if (not ,p)
       ;; (error "Assertion" ',p "failed!")
       (error {Assertion [(U.stringify ',p)] failed!})
       ))

(define java-version>=1_2
  (let ((result 
	 (jsint.Procedure.tryCatch 
	  (lambda () (not (eq? (class "java.lang.reflect.AccessibleObject")
			       #null)))
	  (lambda (e) #f))))
    (lambda () result)))

(define-macro (in-1_2 test . args)
  (if (eq? (java-version>=1_2) test)
      `(begin ,@args)
      '()))

;;;
;;; Utilities.
;;;
(define (print it . arg)
  ;; Like Common Lisp's print.
  (if (null? arg)
      (begin
	(write it)
	(newline))
      (let ((s (car arg)))
	(write it s)
	(newline s)))
  it)

(define-macro (dotimes iters . body)
  ;; Like Common Lisp's (dotimes).
  (let ((var (car iters))
	(max (cadr iters))
	(result (if (pair? (cddr iters)) (caddr iters) '())))
    `(let ((<L> ,max))
       (let <loop> ((,var 0))
	    (if (< ,var <L>)
		(begin ,@body
		       (<loop> (+ ,var 1)))
		,result)))))

(define-macro (dolist iters . body)
  ;; Like Common Lisp's (dolist).
  (let ((var (car iters))
	(items (cadr iters))
	(result (or (and (pair? (cddr iters)) (caddr iters)) '())))
    `(let <loop> ((.items. ,items))
	  (if (null? .items.) ,result
	      (let ((,var (car .items.)))
		,@body
		(<loop> (cdr .items.)))))))

(define (instanceof x c)
  (and (not (eq? x #null))
       (.isAssignableFrom c (.getClass x))))

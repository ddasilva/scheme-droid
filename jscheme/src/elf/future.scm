{
<html> <head>
<title>Futures</title>
</head><body>
<h1>Futures</h1>

<a href="http://dev.acm.org/pubs/articles/journals/toplas/1985-7-4/p501-halstead/p501-halstead.pdf">
 R.H. Halstead, jr.  Multilisp: a language for concurrent symbolic
 computation, ACM Transactions on Programming Languages and
 Systems, Vol. 7, No. 4, October 1985, Pages 501-538</a>.
<p>
 This is a simple implementation of futures in JScheme.

The construct <tt>(future X)</tt> immediately returns a future for the
value of the expression X, and concurrently begins evaluating X in a
separate thread. A future is initially <i>undetermined</i>.  It
becomes <i>determined</i> when its value has been computed.  Use
<tt>(touch f)</tt> to determine the value of a future, f.  <tt>touch</tt>
will suspend until the future is determined.

<p> One issue is how to handle exceptions.  In this version, a future
becomes determined either by producing a value, or throwing an
exception.  A future catches any exception and rethrows it when it is
touched.

<p>
 Procedure names that begin with "%" are private implementation details.
<pre>
}
(define-macro (future exp) `(%make-future (lambda () ,exp)))

(define (determined? f)
  ;; Is future f determined?
  (not (%future-thread f)))
(define (future? f)
  ;; Is Object f a future?
  (and (vector? f) (eq? (vector-ref f 0) 'future)))

(define (touch f)
  ;; If the future has a thread, it means it is not done.
  ;; Use join() to wait for it to be done before fetching the value.
  (if (future? f) 
      `(let ((thread (%future-thread f)))
	 (if thread (.join thread))
	 (if (%future-exception f) (throw (%future-exception f))
	     (%future-value f)))
      f))

;;; `#(future ,thread ,value ,exception)
(define (%allocate-future)(vector 'future #f #f #f))
(define (%future-thread f) (vector-ref f 1))
(define (%future-value f) (vector-ref f 2))
(define (%future-exception f) (vector-ref f 3))

(define (%future-result-set! f v e)
  (synchronize f (lambda (f)
		   (vector-set! f 1 #f)
		   (vector-set! f 2 v)
		   (vector-set! f 3 e))))

(define (%make-future thunk)
  (let* ((fu (%allocate-future))
	 (thread (Thread.
		  (lambda ()
		    (tryCatch (%future-result-set! fu (thunk) #f)
		     (lambda (e)
		       ;; Print exception so user see's it right away.
                       (print e)
		       (%future-result-set! fu #f e)))))))
    (vector-set! fu 1 thread)
    (.start thread)
    fu))

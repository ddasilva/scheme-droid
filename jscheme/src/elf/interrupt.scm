(jlib.Swing.load)

(define (interruptButton)
  ;; Provides an interrupt button for this thread.
  ;; JScheme execution will be interrupted on the next procedure call.
  (let* ((t (Thread.currentThread))
	 (win (window "Interrupt"
		      (border
		       (north (label (.toString t)))
		       (center (button "Interrupt"
				       red
				       (action
					(lambda (e)
					  (Scheme.interrupt t)))))))))
    (.pack win)
    (.show win)))

(define (interruptCheck)
  (if (Thread.interrupted)
      (throw (InterruptedException. "Cooperating Interruption"))))

(define (process i)
  (interruptCheck)
  (print i)
  (if (> i 0) (process (- i 1))
      'done))

(define (withTimeLimit millis thunk default)
  (tryCatch
   (let* ((ct (Thread.currentThread))
	  (t (Thread.
	      (delay
		(tryCatch
		 (begin (Thread.sleep (+ millis 0L))
			(.interrupt ct))
		 identity)))))		; Ignore being interrupted.
     (Thread.interrupted)		; Turn interrupted flag off.
     (.setDaemon t #t)
     (.start t)
     (let ((result (thunk)))
       (.interrupt t)			; Interrupt the sleeping thread.
       result))
   (lambda (e) default)))


(define (withTimeLimit millis thunk default)
  ;; More like R@y's version.
  (let* ((result default)
	 (t (Thread.
	     (delay
	       (tryCatch
		(set! result (thunk))
		(lambda (e)
		  (if (not (instanceof e InterruptedException.class))
		      (throw e))))))))
    (.setDaemon t #t)
    (.start t)
    (.join t millis)
    (if (.isAlive t)
	(begin (.interrupt t)
	       (.join t)
	       default)
	result)))

{
;;; Example
(define x (withTimeLimit 1000L (delay (process 40000)) 'timeout))
}
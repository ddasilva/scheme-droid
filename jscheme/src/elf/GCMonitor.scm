;;;
;;; GC Monitor
;;;
(import "java.awt.BorderLayout")
(import "java.awt.event.WindowEvent")
(import "javax.swing.JFrame")
(import "javax.swing.JLabel")
(import "javax.swing.JPanel")
(import "javax.swing.JProgressBar")
(import "javax.swing.Timer")

(define (for-kvpairs f object kvpairs)
  ;; kvpairs are keyword value pairs: k v ...
  ;; Apply (f object k v) to each pair.
  (let loop ((kvpairs kvpairs))
    (if (null? kvpairs) object
	(begin (f object (car kvpairs) (cadr kvpairs))
	       (loop (cddr kvpairs))))))

(define (with object . kvpairs)
  ;; Use setters as keywords.
  (for-kvpairs (lambda (o k v) (k o v)) object kvpairs))

(define (BorderPanel . kvpairs)
  ;; Create a Panel with BorderLayout with directions as keywords.
  (for-kvpairs (lambda (o k v) (.add o v k)) 
	       (with (JPanel.) .setLayout (BorderLayout.))
	       kvpairs))

(define f1 (java.text.DecimalFormat. "0.0"))
(define mb_1
  ;; Convert total bytes into one decimal place megabytes.
  (lambda (total) (.format f1 (* total 1.0e-6))))

(define (f/ a b)
  ;; Floating point division of 2 longs.
  (/ a (exact->inexact b)))

(define (GCMonitor interval)
  ;; Create the GC Monitor.  Interval is sampling interval in millisec.
  (let* ((pbar (with (JProgressBar.)
		     .setToolTipText "% of memory used"
		     .setStringPainted #t))
	 (label (with (JLabel. "000.0/000.0 000.0")
		      .setToolTipText "Used MB / Total MB KB/sec"))
	 (timer (javax.swing.Timer.
		 interval
		 (let* ((r (Runtime.getRuntime))
			(time0 (System.currentTimeMillis))
			(used0 (- (.totalMemory r) (.freeMemory r))))
		   (Listener.
		    (lambda (ignored)
		      (let* ((total (.totalMemory r))
			     (time1 (System.currentTimeMillis))
			     (used1 (- total (.freeMemory r)))
			     (rate (f/ (- used1 used0) (- time1 time0))))
			(set! time0 time1)
			(set! used0 used1)
			(.setValue pbar (.intValue (/ (* used1 100) total)))
			(.setText label {[(mb_1 used1)]/[(mb_1 total)] [(.format f1 rate)]})))))))
	 (frame (with (JFrame. "GCMonitor")
		      .addWindowListener 
		      (Listener.
		       (lambda (e)
			 (if (= (.getID e) WindowEvent.WINDOW_CLOSING$)
			     (begin (.dispose (.getWindow e))
				    (.stop timer)))))
		      .setContentPane (BorderPanel "West" pbar "East" label))))
    (.start timer)
    (.pack frame)
    (.setVisible frame #t)
    frame))

(GCMonitor 500)

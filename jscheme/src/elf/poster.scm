;;;
;;; Post to a URL.
;;;
(import "jsint.U")
(import "java.io.BufferedReader")
(import "java.io.IOException")
(import "java.io.InputStreamReader")
(import "java.io.PrintStream")
(import "java.net.URL")
(import "java.net.URLConnection")

(define (drain in out)
  ;; Drain stream in to stream out.
  (let ((line (.readLine in)))
    (if (not (eq? line #null))
	(begin (.println out line)
	       (drain in out)))))

(define (post url data)
  ;; Post the data, a printable s-expression, say,  to a URL.
  (let ((con (.openConnection url)))
    (.setDoOutput con #t)
    (.setDoInput con #t)
    (let ((toURL (PrintStream. (.getOutputStream con))))
      (.println toURL (U.stringify data))
      (.close toURL)
      (let ((fromURL (BufferedReader. (InputStreamReader.
				       (.getInputStream con))))
	    (out System.out$))
	(drain fromURL out)
	(.close fromURL)
	))))

;;; Example
(define (rising year month day state place)
  ;; Get sun and moon rising/setting times from USNO.
  (define (data-string year month day state place)
    (string-append
     "FFX=1&ID=AA&xxy=" year "&xxm=" month "&xxd=" day "&st=" state
     "&place=" place "&ZZZ=END"))
  (post (URL. "http://mach.usno.navy.mil/cgi-bin/aa_pap")
	(data-string year month day state place)))

(define (boston-rising date)
  (rising (+ 1900 (.getYear date)) (+ (.getMonth date) 1)
	  (.getDate date) 'MA 'Boston))

(define (tomorrow) (Date. (+ (.getTime (Date.)) 86400000L)))


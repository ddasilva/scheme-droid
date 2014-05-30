
 ;; storing session data
 (define (session-set request N V)
   (.setAttribute (.getSession request) N V))

 (define (session-get request N Default)
  (let ((x (.getAttribute (.getSession request) N)))
   (if (equal? x #null) Default x)))

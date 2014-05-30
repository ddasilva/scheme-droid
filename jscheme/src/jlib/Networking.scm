
(define (make-group-client name group host port)
    
      (define debugging #t)
      (define debugta (textarea 10 40)) (define debugwin (window "debug" debugta))
      (define printdebug (lambda R (if debugging (begin (writeexpr debugta R)(.pack debugwin) (.show debugwin)))))
      (define (toStringQ S)  (jscheme.REPL.printToString S #t))
      (define string->exprlist jscheme.REPL.parseScheme)
    
      (define s (java.net.Socket. host port))

      (define in (java.io.BufferedReader.
                           (java.io.InputStreamReader. 
                             (.getInputStream s))))
      (define out (java.io.PrintStream.
                            (.getOutputStream s)))
    
    
      (define (group-thread-handler)
         (let loop ()
              (let ((x (tryCatch (first (string->exprlist (.readLine in))) (lambda(e) ()))))
                (if (pair? x)
                   (begin
                      (tryCatch 
                        (begin
                            (apply-super-listeners (first x) (rest x) super-listeners)
                            (apply-listeners (first x) (rest x) listeners))
                        (lambda(e) 
                            (warning (list "group-thread-handler error: " e))))
                      (loop))
                   (.close s)))))


    
      (define listeners ())

      (define (apply-listeners Key Val Listeners)
        (for-each 
             (lambda(x) 
                (if (equal? (first x) Key) (tryCatch (apply (second x) (cons Key Val)) (lambda(e) (printdebug e) #t))))
             Listeners))
    
      (define super-listeners ())

      (define (apply-super-listeners Key Val Listeners)
        (for-each 
             (lambda(x) 
                (tryCatch (apply (first x) (cons Key Val)) (lambda (e)(printdebug e) #t)))
             Listeners))
    

      (define handler-thread (java.lang.Thread. group-thread-handler))

      (define G 
          (lambda R
            (case (first R)
    
              ((add-listener) 
               (set! listeners (cons (rest R) listeners)))

              ((add-super-listener) 
               (set! super-listeners (cons (rest R) super-listeners)))
    
              ((logout) 
               (.close s))
    
              ((send) 
               (.println out (toStringQ (rest R))))
    
              ((get)
               (case (second R)
                ((name) name)
                ((group) group)
                ((address) (list host port))
                ((socket-info) (list s in out))
                ((listeners) listeners)
                ((thread) handler-thread)
                (else (warning (list "ERROR: unknown variable " (second R))))))
    
              (else
                (warning (list "unknown command" R))))))
    
    
    
    
      (define (send-to-group User Rest)
        (tryCatch
         (let ((s (java.net.Socket. (first User) (second User))))
            (let ((in (java.io.BufferedReader.
                         (java.io.InputStreamReader. 
                             (.getInputStream s))))
                  (out (java.io.PrintStream.
                          (.getOutputStream s))))
             (.println out "message") 
             (.println out (toStringQ Rest)))
           (.close s))
        (lambda(e) 
           (warning (list "error during send-to-user" User Rest e)))))
    
      (define (warning X) 
        (printdebug X) "warning"
      )
        
  (define theport (.getPort s))

  (display (list "group-client joining group" name group)) (newline)
  (.start handler-thread)
  (.println out "join")
  (.println out name)
  (.println out group)
  G
)


(define (send-line host port msg)
   (let* (
          (s (java.net.Socket. host port))
          (theport (.getLocalPort s))
          (in (java.io.BufferedReader.
               (java.io.InputStreamReader. 
                (.getInputStream s))))
          (out (java.io.PrintStream.
                (.getOutputStream s))))
     (.println out msg)
     (let ((x (.readLine in)))
       (.close s)
       x)))




(define (make-servent port)
  (define (toStringQ S)  (jscheme.REPL.printToString S #t))
  (define string->exprlist jscheme.REPL.parseScheme)
  (define ss (java.net.ServerSocket. port))

  (define (start-server)
    (let serverloop ((s (.accept ss)))
        (.start (java.lang.Thread. (lambda() (handle-socket s))))
        (serverloop (.accept ss))))

  (define listeners ())
  (define groups-I-own ())
  (define groups-I-have-joined ())

  (define (handle-socket s)
         (tryCatch
          (begin 
           (let* (
                  (in (java.io.BufferedReader.
                       (java.io.InputStreamReader. 
                         (.getInputStream s))))
                  (out (java.io.PrintStream.
                        (.getOutputStream s)))
                  (message-type (.readLine in)))
             (case message-type
              (("message")
               (let ((theData 
                       (first 
                         (string->exprlist 
                            (tryCatch 
                               (.readLine in) 
                               (lambda(e) (display (list "I/O error" e))(.close s)(throw e)))))))
                   (apply-super-listeners (first theData) (rest theData) super-listeners)
                   (apply-listeners (first theData) (rest theData) listeners)
                  ))

              (("ping")
                (.println out (.getHostAddress (.getInetAddress s))))

              (else (warning (list 'unknown-message-type message-type))))
             (.close in)(.close out) (.close s)
          ))
          (lambda(e) (.close s) (warning (list "exception in handle socket " s e)) )))


  (define (apply-listeners Key Val Listeners)
    (for-each 
         (lambda(x) 
            (if (equal? (first x) Key) (tryCatch (apply (second x) (cons Key Val)) (lambda(e) (printdebug (list "error" e))))    ))
         Listeners))


   (define super-listeners ())

   (define (apply-super-listeners Key Val Listeners)
        (for-each 
             (lambda(x) 
                (tryCatch (apply (first x) (cons Key Val)) (lambda(e) (printdebug (list "error" e)))))
             Listeners))
    


  (define N 
    (let ((HOSTADDRESS (tryCatch (.getHostAddress (java.net.InetAddress.getLocalHost)) (lambda(e) "localhost"))))
      (lambda R
        (case (first R)

          ((add-listener) 
           (set! listeners (cons (rest R) listeners)))

          ((add-super-listener) 
           (set! super-listeners (cons (rest R) super-listeners)))

          ((logout) 
           (.close ss))

          ((send) 
           (if (equal? (second R) 'all)
               (broadcast-message (rest R))
               (send-to-user (second R) (rest (rest R)))))
          ((get)
           (case (second R)
            ((address) (list HOSTADDRESS (.getLocalPort ss)))
            ((port) (.getLocalPort ss))
            ((ip) (.getHostAddress (.getInetAddress ss)))
            ((listeners) listeners)
            ((socket-server) ss)
            (else (warning (list "ERROR: unknown variable " (second R))))))

          ((set)
           (case (second R)
             ((host) (set! HOSTADDRESS (third R)))
             (else (warning (list "unknown command" R)))))

          (else
            (warning (list "unknown command" R)))))))



  (define (broadcast-message R)
    (warning "this has not yet been implemented"))

  (define (send-to-user User Rest)
    (tryCatch
     (let ((s (java.net.Socket. (first User) (second User))))
        (let ((in (java.io.BufferedReader.
                     (java.io.InputStreamReader. 
                         (.getInputStream s))))
              (out (java.io.PrintStream.
                      (.getOutputStream s))))
         (.println out "message") 
         (.println out (toStringQ Rest)))
       (.close s))
    (lambda(e) 
       (warning (list "error during send-to-user" User Rest e)))))


  (define (warning X) 
    (printdebug X) "warning")

  (define theport (.getLocalPort ss))
    
  (.start (java.lang.Thread. (lambda() (start-server))))
  N
)


(define (make-group-server port)
    
    (define (toStringQ S)  (jscheme.REPL.printToString S #t))
    (define string->exprlist jscheme.REPL.parseScheme)

    (define (create-peer group in out s username )
         (list group in out s username (java.lang.System.currentTimeMillis))) 
    
    (define (update-peer-time peer)
      (set-car! (list-tail peer 5) (java.lang.System.currentTimeMillis)))
    (define (get-peer-groups peers) (rem-dups (map first peers) ()))
    (define (get-peer-users peers) (rem-dups (map (lambda(x) (list-ref x 4)) peers) ()))
    (define (get-peer-users-in-group peers group)
      (filter (lambda(x) (if (equal? (first x) group) (list-ref x 4) #f)) peers))
    
    (define (filter F L)
      (if (null? L) ()
         (let ((x (F (first L))))
            (if x (cons x (filter F (rest L))) (filter F (rest L))))))
    
    (define (remove-peer username group P)
       (cond 
          ((null? P) ())
          ((and (equal? username (list-ref (first P) 4)) (equal? group (first (first P))))
           (remove-peer username group (rest P)))
          (else (cons (first P) (remove-peer username group (rest P))))))
    
    (define (broadcast-to-peers peers message)
      (for-each (lambda(x) (.println (third x) (jsint.U.stringify message))) peers))  
    
    (define (broadcast-to-group peers group theData)
     (map (lambda(x) 
            (if (equal? (first x) group)
                (begin
                   (.println (third x) theData)
                   (.flush (third x)))))
          peers)
    )
    
    (define peers ())
    (define count 0)
    (define ss (java.net.ServerSocket. port))

    (define (start-server port)
        (let serverloop ()
           (let ((s (.accept ss)))
            (.start (java.lang.Thread. (lambda()
					 (tryCatch
              (begin 
               (let* (
                      (in (java.io.BufferedReader.
                           (java.io.InputStreamReader. 
                             (.getInputStream s))))
                      (out (java.io.PrintStream.
                            (.getOutputStream s)))
                      (action (.readLine in)))

                 (case action
    
                  (("join")
                    (let* (
                      (username (.readLine in))
                      (group (.readLine in))
                      (peer (create-peer group in out s username))
                      (forwarding-thread 
                            
                       (java.lang.Thread.
                          (lambda() 
                           (let loop ((c 0))
                             (let ((theData (tryCatch (.readLine in) (lambda(e) #null))))

                               (cond 
                                 ((equal? theData #null)
                                     
                                     (synchronize 'p
                                       (lambda(p) (set! peers (remove-peer username group peers)))))
                                 ((equal? theData "")
                                  
                                  (loop (+ c 1))) 
                                 (else
                                     (set! count (+ 1 count))
                                     
                                     (broadcast-to-group peers group theData)
                                     (loop (+ c 1)))))))
                           username)))


                        (synchronize 'p (lambda(p) 
                           
                           (set! peers (cons peer peers))))

                        (.start forwarding-thread)))
    
                  (("groups")
                    
                    
                    (.println out (jsint.U.stringify (get-peer-groups peers)))
                    (.println out "\n")
                    (.flush out)   
                    (.close s)
                   )
    
                  (("users")
                    
                    
                    (.println out (jsint.U.stringify (get-peer-users peers)))
                    (.println out "\n")
                    (.flush out)   
                    (.close s)
                   )
                  (("users-in-group")
                    
                    
                    (.println out (jsint.U.stringify (get-peer-users-in-group peers (.readLine in))))
                    (.println out "\n")
                    (.flush out)   
                    (.close s)
                   )
    
                  (("quit")
                    (java.lang.System.exit 0))
    
                  (else
                    (.println out "(ERROR unknown action:)")
                    (.println out action)
                    (.flush out)
                    (.close s)))))
                (lambda(e) (printdebug (list "ERROR: " e)) ))
               ))))
             
             (serverloop)  ))
    
           
    
    (define (rem-dups L S)
      (if (null? L) S
        (let ((x (first L)))
          (if (member x S)
              (rem-dups (rest L) S)
              (rem-dups (rest L) (cons x S))))))
    
    
    (define server-thread 
       (java.lang.Thread. (lambda() (start-server port))))

    (.start server-thread)

  (lambda R
     (case (first R)
       ((get)
           (case (second R)
              ((thread) t)
              ((peers) peers)
              ((count) count)
              ((ss) ss)
              ((port) (.getLocalPort ss))
              (else (list "don't understand" R))))
       ((quit)
          (.close ss)
          (.stop server-thread))))
)



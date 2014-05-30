;; SNLP.scm


(define (html_unquote x) 
  (define s (java.util.StringTokenizer. x "&;" #t))
  (define (transform  x y z rest)
     (if (not (and (equal? x "&") (equal? z ";"))) 
         (if (not(.hasMoreTokens rest))
             (list x y z)
             (let ((w (.nextToken rest)))
                (cons x (transform y z w rest))))
         (case y
            (("amp") (cons "&" (start-transform rest)))
            (("lt") (cons "<" (start-transform rest)))
            (("gt") (cons ">" (start-transform rest)))
            (("quot") (cons "\"" (start-transform rest)))
            (else 
               (if (not (.hasMoreTokens rest))
                   (list x y z)
                   (let ((w (.nextToken rest)))
                     (cons x (transform y z w rest))))))))
   (define (start-transform s)
      (if (not (.hasMoreTokens s))
          ()
          (let ((x (.nextToken s)))
            (if (not (.hasMoreTokens s))
                (list x)
                (let ((y (.nextToken s)))
                   (if (not (.hasMoreTokens s))
                      (list x y)
                      (let ((z (.nextToken s)))
                          (transform x y z s))))))))
   (apply string-append (start-transform s)))

(define (my-map F L)
   (if (null? L) ()
       (let ((y (F (first L))))
          (cons y (my-map F (rest L))))))

(define (main argarray)
  (.pack snlpwin)
  (.show snlpwin)
  (let ((args (array->list argarray)))
    (my-map eval (string->exprlist (html_unquote (first args))))))

(define (string->expr x)  
    (if (equal? x #null) #null (first (jscheme.REPL.parseScheme x))))

(define addquitaction 
      (lambda(x)
        (.addWindowListener x (jsint.Listener. (lambda(e)
          (if 
             (or
               (equal? (.getID e) java.awt.event.WindowEvent.WINDOW_CLOSING$)
               (equal? (.getID e) java.awt.event.WindowEvent.WINDOW_CLOSED$))
             (java.lang.System.exit 0)))))))


(define snlpwin 
  (let ((win (java.awt.Frame. "snlp monitor")))
    (.add win (java.awt.Label. "Closing this window will shutdown the snlp application"))
    (addquitaction win)
    win))

(define (string->exprlist x)  
    (if (equal? x #null) #null (jscheme.REPL.parseScheme x)))


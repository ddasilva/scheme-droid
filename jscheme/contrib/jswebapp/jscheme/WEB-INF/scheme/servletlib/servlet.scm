(define (warning-webpage  b)
 {<html><head><title>WARNING</title></head>
    <body style="background:black; color:red; font: bold 18pt sans-serif">
     [b]</body></html>})

(define (basic-webpage t b)
 {<html><head><title>[t]</title></head>
    <body style="background:white; color:black">
     [b]</body></html>})

(define (css-webpage t css b)
 #{<html><head><title>#[t]#</title><style type="text/css">#[css]#</style></head>
    <body>
     #[b]#
   </body>
</html>
}#)

(define-macro (servlet Vars . Body)
  `(let ,(map (lambda(x) `(,x (.getParameter request ,(.toString x)))) Vars) ,@Body))

(define (html_quote x) 
    (define s (java.util.StringTokenizer. x "<>&\"" #t))
    (define (maketokens s)
           (if (not (.hasMoreTokens s)) ()
               (let ((x (.nextToken s)))
                 (cons
                   (case x
                      (("<")  "&lt;")
                      ((">") "&gt;")
                      (("\"") "&quot;")
                      (("&") "&amp;")
                      (else x))
                   (maketokens s)))))

    (apply string-append (maketokens s))
 )



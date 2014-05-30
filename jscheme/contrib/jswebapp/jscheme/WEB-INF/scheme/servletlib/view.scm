;; This reads a file and shows the source ...
;; It will let you show the source code of any file on your computer!!

(define (viewCode file)

 (define (read-all in)
    (define (drain in sbuffer)
      (let ((line (.readLine in)))
        (if (eq? line #null)
            (.toString sbuffer)
            (drain in (.append (.append sbuffer line) "\n")))))
    (drain in (java.lang.StringBuffer.)))

    (define code (read-all (java.io.BufferedReader. (java.io.FileReader. {webapps[context]/[file]}))))

    {<html>
      <body>
       <xmp>[code]</xmp>
      </body>
     </html>})


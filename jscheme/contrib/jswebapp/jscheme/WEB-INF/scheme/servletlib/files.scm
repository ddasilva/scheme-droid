;; files.scm
;; File I/O for servlets, but note ...
;; these may not be thread-safe....

(define (write-to-file filename data)
   (let (
         (f (java.io.PrintWriter. (java.io.FileWriter. filename #f)))
        )
      (.println f (jscheme.REPL.printToString  data #t))
      (.close f)))

(define (append-to-file filename data)

   (let (
         (f (java.io.PrintWriter. (java.io.FileWriter. filename #t)))
        )
      (.println f  (jscheme.REPL.printToString data #t))
      (.close f)))

(define (read-from-file name default)
   (tryCatch
     (let* (
         (f (open-input-file name))
         (data (read f)))
       (.close f)
       data)
     (lambda(e) default)))

(define (read-all-from-file name default)
   (tryCatch
     (let* (
         (f (open-input-file name))
         (data
          (let loop ((x (read f)))
             (if (eof-object? x) ()
                 (cons x (loop (read f)))))))
       (.close f)
       data)
     (lambda(e) default)))

(define (read-string-from-file name default)
   (tryCatch
     (let* (
         (f (java.io.BufferedReader. (java.io.FileReader. name)))
         (data
          (let loop ((x (.readLine f)))
             (if (equal? x #null) ()
                 (cons (string-append x "\n") (loop (.readLine f)))))))
       (.close f)
       (apply string-append data))
     (lambda(e) default)))

(define (servlet-file request name)
  {[(getRealPath (.toString (.getServletPath request)))]_[name]})
;  {webapps[(.getRequestURI request)]_[name]}  )

;; this is for servlet-inserts ..
(define (include-file F)
    (read-string-from-file (getRealPath F) ""))

(define (include-absolute-file F)
    (read-string-from-file (.getAbsolutePath (java.io.File. F))))





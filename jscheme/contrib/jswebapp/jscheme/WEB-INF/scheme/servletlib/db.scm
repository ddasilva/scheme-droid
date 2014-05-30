;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  db.scm
;;   Here we create a generic library for accessing a database
;;   (which must be either a mysql or an hsqldb database)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; We can use either 
;;   mysql 
;;  or
;;   hsdqld

;(define database 'mysql)
(define database 'hsqldb)

;; This defines the proper driver
(define jdbc-driver 
  (case database
     ((mysql) "com.mysql.jdbc.Driver") ;; this is the official mysql driver
     ((hsqldb) "org.hsqldb.jdbcDriver") ;; this is the hsqldb driver
  ))


(define jdbc-driver-class (java.lang.Class.forName jdbc-driver))

;; connect to a database using the preloaded jdbc drivers
(define (connect host/db user pw) 
  (let* ((d (java.lang.Class.forName jdbc-driver)))
     (java.sql.DriverManager.getConnection host/db user pw)))

;; send a query to the database and get result as list of lists
;; first list is the meta data giving column names
(define (handlequery con query)
  (let* ((stmt (.createStatement con))
         (rs (.executeQuery stmt query))
         (rsmd (.getMetaData rs))
         (cols (.getColumnCount rsmd))
         (result
           (cons
              (let loop1 ((i 1))
                  (if (> i cols) ()
                      (cons (.getColumnLabel rsmd i) 
                            (loop1 (+ i 1)))))
              (let loop ()
                   (if (not (.next rs)) ()
                       (cons
                          (let innerloop ((i 1))
                              (if (> i cols) ()
                                  (cons (.getString rs i) 
                                        (innerloop (+ i 1)))))
                          (loop)))))))
      (map .close (list rs stmt))
     result))

;; open a connection, run a querym, get back a list of lists, and close the connection
(define (runquery host/db user pw query)
  (let* ((con (connect host/db user pw))
         (results (handlequery con query)))
    (.close con)
    results))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these are some quoting procedures for various flavors of SQL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (toMYSQL x) ;; this generates an SQL quoted string from a JScheme string
                    ;; e.g. a'\b"c --> 'a\'\\b\"c'
        (define s (java.util.StringTokenizer. x "\\'\"" #t))
        (define (maketokens s)
           (if (not (.hasMoreTokens s)) ()
               (let ((x (.nextToken s)))
                 (cons
                   (case x
                      (("\\")  "\\\\")
                      (("'") "\\'")
                      (("\"") "\\\"")
                      (else x))
                   (maketokens s)))))
         (apply string-append `("'" ,@(maketokens s) "'") ))


  (define (toHSQL x) ;; this generates an HSQL quoted string, with all (')s doubled (''),
                     ;; e.g. a'\b"c --> 'a''\b"c'
        (define s (java.util.StringTokenizer. (string-append x) "\\'\"" #t))
        (define (maketokens s)
           (if (not (.hasMoreTokens s)) ()
               (let ((x (.nextToken s)))
                 (cons
                   (case x
                      (("'") "''")
                      (else x))
                   (maketokens s)))))
         (apply string-append `("'" ,@(maketokens s) "'") ))


;; This is the SQL quoter you should use to be database independent
(define toSQL 
  (case database
     ((mysql) toMYSQL) ;; select quoting procedure
     ((hsqldb) toHSQL)
  ))



;;; Example use of JDBC.
;;; For this to work with Oracle you need Oracle's jdbcthin\lib\classes111.zip
;;; in your class path.  ODBC support is built in to JDK 1.2.
(import "java.math.BigDecimal")
(import "java.sql.DriverManager")

(define (map-columns f row result n)
  ;; Map (f row n) over the n columns of row putting the result in 
  ;; the vector result.
  (if (= n 0) result
      (let ((i (- n 1)))
	(vector-set! result i (f row n))
	   (map-columns f row result i))))

(define (get-datum row n)
  ;; Get a cell value.
  (let ((it (.getObject row n)))
    (if (eq? it #null) it
	(datum-value it))))

(define-method (datum-value (value java.lang.Object)) value)
;;; KRA 03MAR00: Strings are often duplicates, so intern them all.
(define-method (datum-value (value String)) (.intern value))
(define-method (datum-value (value java.math.BigDecimal))
  ((if (= (.scale value) 0)
       .longValue
       .doubleValue)
   value))

;;; KRA 01DEC03: Mysql returns byte[] not a String.
(define-method (datum-value (value byte[])) (String. value))

(define (map-query f con query)
  ;; Collect a list of vectors representing the results of a query.
  (let* ((stmt (.createStatement con))
	 (result (.executeQuery stmt query))
	 (items '()))
    (iterate result
	     (lambda (row) (set! items (cons (f row) items))))
    (.close stmt)
    (reverse items)))

(define (collect-query con query)
  ;; Collect a list of vectors representing the results of a query.
  (let* ((stmt (.createStatement con))
	 (result (.executeQuery stmt query))
	 (brick  (result-brick result)))
    (.close stmt)
    brick))
    
(define (result-brick result)
  ;; Convert a result set into standard form.
  (let* ((meta (.getMetaData result))
	 (ncol (.getColumnCount meta))
	 (col (map-columns (lambda (row n) (.getColumnName row n))
			   meta (make-vector ncol) ncol))
	 (data '()))
    (iterate
     result
     (lambda (result)
       (set! data (cons (map-columns get-datum result (make-vector ncol) ncol)
			data))))
    (cons col (reverse data))))

(define-method (iterate (result java.sql.ResultSet) f)
  (let loop ()
    (if (.next result)
	(begin (f result)
	     (loop)))))

;;; Example:
;;; Register the driver. 
(define (register-driver type)
  (DriverManager.registerDriver
   ((case type
      ;; Put jar in class path, lazy loading won't work.
      ((oracle) oracle.jdbc.driver.OracleDriver.) ; Needs a jar.
      ((odbc) sun.jdbc.odbc.JdbcOdbcDriver.) ; Comes with JDK.
      ((instantdb) org.enhydra.instantdb.jdbc.idbDriver.)
      (else (error "Unknown driver type " type))))))

;;; Connect to Stout's database
'(define dart-con (DriverManager.getConnection 
	     "jdbc:oracle:thin:@stout:1521:dart" "user" "password"))

;;; Sample queries.
'(define countries (collect-query dart-con "select unique(country_state_code), country_state_long_name from geoloc"))

(define (describe-table con table-name)
  ;; Describe a table.  may not be portable, but works in Oracle.
  (collect-query 
   con 
   (string-append
    "SELECT COLUMN_NAME, DATA_TYPE, DATA_LENGTH, DATA_SCALE FROM ALL_TAB_COLUMNS WHERE TABLE_NAME = '" table-name "'")))

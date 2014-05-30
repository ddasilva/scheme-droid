;;; This is a portable way of running md5.  It produces a byte array
;;; that you can turn into a printable string.

(load "elf/basic.scm")
(import "java.security.MessageDigest")
(import "java.security.DigestOutputStream")
(import "java.io.PrintWriter")
(import "java.lang.String")
(import "java.lang.StringBuffer")
(import "java.lang.reflect.Array")
(import "elf.NullOutputStream")

(define-method (md5 (x Object))
  (call-with-md5-writer (lambda (w) (write x w))))

(define-method (md5 (f java.io.File))
  (md5 (BufferedReader f)))

(define-method (md5 (r java.io.BufferedReader))
  (call-with-md5-writer
   (lambda (w) (iterate r (lambda (L) (.println w L))))))

(define-method (BufferedReader (f java.io.File))
  (java.io.BufferedReader. (java.io.FileReader. f)))

(define (call-with-md5-writer f)
  (let* ((md (MessageDigest.getInstance "MD5"))
	 (md-writer (PrintWriter. (DigestOutputStream. (NullOutputStream.) md))))
    (f md-writer)
    (hexify-md5 (.digest md))))

(define (hexify-md5 d)
  (do ((i 0 (+ i 1))
       (d-hex (StringBuffer.) d-hex))
      ((= i (Array.getLength d)) (String. d-hex))
    (set! d-hex (.append d-hex (byte->hex (Array.get d i))))))

(define byte->hex
  (let ((table (make-vector 256)))
    (define (byte->hex0 b)
      (let* ((chars "0123456789abcdef")
	     (b (if (< b 0) (+ b 256) b))
	     (low (modulo b 16))
	     (hi  (inexact->exact (/ b 16)))
	     (sb (StringBuffer. 2)))
	(.append (.append sb (string-ref chars hi)) (string-ref chars low))))
    (let loop ((i 0))
      (if (< i 256)
	  (begin (vector-set! table i (byte->hex0 i))
		 (loop (+ i 1)))))
    (lambda (b)
      (vector-ref table (if (< b 0) (+ b 256) b)))))


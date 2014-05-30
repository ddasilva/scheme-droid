;;;
;;; Iterators
;;;

(import "java.lang.Object")
(import "java.lang.String")

(use-module "elf/util.scm" 'import 'all)

(define-method (iterate (mapper jsint.Procedure) action)
  (mapper action))

;;; Hashtable and Vector specialization are only needed for JDK 1.1
(define-method (iterate (items java.util.Hashtable) action)
  (iterate (.elements items) action))

(define-method (iterate (items java.util.Vector) action)
  (iterate (.elements items) action))

(define-method (iterate (items java.util.Enumeration) action)
  (let loop ()
    (if (.hasMoreElements items)
	(begin (action (.nextElement items))
	     (loop)))))

(when-classes
 (java.util.Collection)
 (define-method (iterate (items java.util.Map) action)
   (iterate (.values items) action))

 (define-method (iterate (items java.util.Iterator) action)
   (let loop ()
     (if (.hasNext items)
       (begin (action (.next items))
	      (loop)))))

 (define-method (iterate (items java.util.Collection) action)
   (iterate (.iterator items) action))
 )

(define-method (iterate (items jsint.Pair) action)
  (let loop ((items items))
    (if (pair? items)
	(begin
	  (action (car items))
	  (loop (cdr items))))))

(define-method (iterate (items Object[]) action)
  (let loop ((i 0) 
             (L (vector-length items)))
    (if (< i L) (begin (action (vector-ref items i)) (loop (+ i 1) L)))))

(define-method (iterate (items String) action)
  (let loop ((i 0) 
             (L (string-length items)))
    (if (< i L) (begin (action (string-ref items i)) (loop (+ i 1) L)))))

(define-method (iterate (items Object) action)
  (if (.isArray (.getClass items))
      (let loop ((i 0) 
		 (L (java.lang.reflect.Array.getLength items)))
	(if (< i L)
	    (begin (action (java.lang.reflect.Array.get items i))
		   (loop (+ i 1) L))))
      (error "Don't know how to iterate over " items)))

(define-method (iterate (items java.io.BufferedReader) action)
  ;; Iterate over the lines of a buffered reader.
  (let loop ((it (.readLine items)))
    (if (not (eq? it #null))
	(begin
	  (action it)
	  (loop (.readLine items))))))

(define-method (iterate (items javax.swing.text.ElementIterator) action)
  ;; Unfortunately, this Class is not an iterator!
  (let loop ((item (.next items)))
    (if (not (isNull item))
	(begin (action item)
	       (loop (.next items))))))

(define (map* f xs)
  ;; Like map but works for any container that iterate works on.
  ;; KRA 13MAY00: +++ Someday rewrite without reverse.
  (let ((results '()))
    (iterate xs (lambda (x) (set! results (cons (f x) results))))
    (reverse results)))

(define (for-each* f xs)
  ;;; Like for-each but generalized for any container that iterate works on.
  (iterate xs f))

;;;
;;; Fold to the left.
;;;
(define (foldL xs how so-far)
  (if (isNull xs) so-far
      (begin
	(iterate xs (lambda (x) (set! so-far (how x so-far))))
	so-far)))

(define identity (lambda (x) x))

(define (keep test)
  (lambda (it sofar)
    (if (test it) (cons it sofar) sofar)))

(define (find p xs)
  ;; Find first x of xs satisfying (p x).
  (call/cc (lambda (return)
	     (iterate xs (lambda (x) (if (p x) (return x)))))))
(assert (= (find odd? '(2 4 6 3 5)) 3))
(assert (= (find odd? #(2 4 6 3 5)) 3))

;;; KRA 23JUL04: filter-in is deprecated.  Use filter.
(define (filter-in p xs) (foldL xs (keep p) '()))

(assert (equal? (filter-in identity #null) '()))

(assert
 (equal? (filter-in symbol? '(3 + x f define)) 
	 '(define f x +)))

;;; KRA 21APR02: filter-in reverses the list of kept things.
;;; (filter) keeps items in their original order.
(define (filter keep? xs) (reverse (filter-in keep? xs)))

(define (some p xs)
  (call/cc (lambda (return)
	     (iterate xs (lambda (x) (if (p x) (return #t))))
	     #f)))

(define (every p xs)
  (call/cc (lambda (return)
	     (iterate xs (lambda (x) (if (not (p x)) (return #f))))
	     #t)))

(define (crack string by)
   (map* identity (StringTokenizer. string by)))
(assert (equal? (crack "foo/bar/baz" "/") '("foo" "bar" "baz")))

(define (separate by items)
  ;; (separate "," '(1 2 3)) -> (1 "," 2 "," 3)
  (define (separate0 head tail)
    (if (null? tail) (list head)
	(cons head (cons by (separate0 (car tail) (cdr tail))))))
  (if (null? items) items
      (separate0 (car items) (cdr items))))
(assert (equal? (separate "," '(1 2 3)) '(1 "," 2 "," 3)))
(assert (equal? (separate "," (crack "foo/bar/baz" "/"))
		'("foo" "," "bar" "," "baz")))

(define flatten
  ;; Flatten a list of lists into a list of atoms.  Tail recursive version.
  ;; Can we cons less than this?
  ;; Can we use foldL?
  (let ()				; Lambda lift.
    (define (flatten1 result xs)
      (if (null? xs) (reverse result)
	  (let ((head (car xs))
		(xs (cdr xs)))
	    (cond ((null? head) (flatten1 result xs))
		  ((pair? head)
		   (flatten1 result (cons (car head) (cons (cdr head) xs))))
		  (else (flatten1 (cons head result) xs))))))
    (lambda (xs) (flatten1 '() xs))))

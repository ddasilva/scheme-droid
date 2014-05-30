{

These utilities let you play with data in a somewhat relational way.
Here's a translation from scheme to the SQL equivalent.

Assume data is a table (in SQL) or a collection in JScheme.

(filter p data)                select * from data where p

(map* (project x y z) data)    select x, y, z from data

(sort data (comparator < x))   select * from data order by x

(map* (project (unique car) (count cadr))
      (group-by (project x y) data)
                               select unique(x), count(y) from data
			       group by x, y

Group-by example - print the names of methods that are defined in both
JButton and JFrame classes:

  (for-each
   print
   (map (unique .getName)
	(filter (lambda (x) (> (length x) 1))
		(group-by .getName
			  (flatten
			   (map vector->list
				(map .getDeclaredMethods
				     (list javax.swing.JButton.class
					   javax.swing.JFrame.class))))))))

Histogram example - print a histogram of the number of arguments to
methods of the JButton class:

  (for-each print
	    (histogram
	     (lambda (m) (vector-length (.getParameterTypes m)))
	     (.getMethods javax.swing.JButton.class)))

Histogram example - print a histogram of the return types of methods of
the JButton class:

  (for-each print
	    (histogram .getReturnType (.getMethods javax.swing.JButton.class)))

}

(use-module "elf/util.scm" 'import 'all)
(use-module "elf/iterate.scm" 'import 'all)

(define (comparator compare access)
  ;; Construct a java.util.Comparator from a comparison function and
  ;; an accessor.  For example (comparator string<? .getName).
  (elf.SchemeComparator.
   (lambda (a b)
     (let ((a (access a))
	   (b (access b)))
       (if (compare a b) -1
	   (if (compare b a) 1
	       0))))))

(define-method (sort (xs jsint.Pair) comparator)
  (let ((it (list->vector xs)))
    (sort it comparator)
    (vector->list it)))

(define-method (sort (xs Object[]) comparator)
  (Arrays.sort xs comparator))

(define (by n xs)
  ;; Group the elements of the list xs into groups of n elements.
  (define (by0 i xs items so-far)
    (if (= i 0) (by0 n xs '() (cons (reverse items) so-far))
	(if (null? xs) (finish items so-far)
	    (by0 (- i 1) (cdr xs) (cons (car xs) items) so-far))))
  (define (finish items so-far)
    (reverse
     (if (null? items) so-far
	 (cons (reverse items) so-far))))
  (assert (> n 0))
  (by0 n xs '() '()))

(assert (equal? (by 2 '(1 a 2 b 3 c)) '((1 a) (2 b) (3 c))))

(define (group-by what xs)
  (let ((table (java.util.Hashtable.)))
    (iterate xs
	     (lambda (x)
	       (let* ((key (what x))
		      (items (.get table key)))
		 (.put table key (cons x (if (eq? items #null) '() items))))))
    (.values table)))

(define (unique p)
  ;; like select unique(p) in SQL.
  (lambda (xs) (p (call/cc (lambda (return)
			     (iterate xs (lambda (x) (return x))))))))

(define (project . ps)
  (lambda (x) (map (lambda (p) (p x)) ps)))

(define (count xs)
  (let ((c 0))
    (iterate xs (lambda (x) (set! c (+ c 1))))
    c))

;; Turn a histogram into an HTML table.
;; Does 2 columns only
(define (histogram-table name rows)
  {<table border=1>
  <tr><td align=right>Count</td> <td align=left>[name]</td></tr>
  [(map (lambda (row) {
  <tr><td align=right>[(car row)]</td> <td align=left>[(.toString (cadr row))]</td></tr>\n})
	rows)]
</table>})

(define (default what value)
  ;; Default value provider.  Returns an accessor that either returns
  ;; (what x) or value if (what x) isNull.
  (lambda (x)
    (let ((it (what x)))
      (if (isNull it) value it))))

(define (histogram what items)
  ;; This version is short but does more work.
  (let ((what (default what 'none)))
    (sort (map* (project length (unique what)) (group-by what items))
	  (comparator > car))))

(define (histogram what items)
  ;; This version uses the key and value of the HashMap and doesn't
  ;; collect the items in a group.
  (let ((what (default what 'none))
	(table (java.util.HashMap.)))
    (iterate items
	     (lambda (x)
	       (let* ((key (what x))
		      (count (.get table key)))
		 (.put table key (+ (if (eq? count #null) 0 count) 1)))))
    (sort (map* (lambda (e) (list (.getValue e) (.getKey e)) )
		(.entrySet table))
	  (comparator > car))))

(define (log2 x) (/ (log x) (log 2)))

(define (entropy histogram)
  ;; Think of a histogram as a probability distribution over the
  ;; symbols in a stream of data.  The entropy describes its shape of
  ;; that distribution and the average number of bits per symbol
  ;; needed to encode it.
  (let* ((data (map car histogram))
	 (total (+ 0.0 (apply + data))))
    (define (e n)
      (let ((p (/ n total)))
	(- (* (log2 p) p))))
    (apply + (map e data))))

{
This is a very nice merge sort from Dybvig's The Scheme language
second edition: http://www.scheme.com/tspl2d/examples.html#g2335
The input list is copied once.

The merge sorting algorithm works quite simply. The input list is
split into two approximately equal sublists. These sublists are sorted
recursively, yielding two sorted lists. The sorted lists are then
merged to form a single sorted list. The base cases for the recursion
are lists of one and two elements, which can be sorted trivially.
}

(define merge-sort #f)
(define merge #f)
(let ()
  (define dosort
    (lambda (pred? ls n)
      (cond
        ((= n 1) (list (car ls)))
        ((= n 2) (let ((x (car ls)) (y (cadr ls)))
                   (if (pred? y x) (list y x) (list x y))))
        (else
         (let ((i (quotient n 2)))
           (domerge pred?
                    (dosort pred? ls i)
                    (dosort pred? (list-tail ls i) (- n i))))))))
  (define domerge
    (lambda (pred? l1 l2)
      (cond
        ((null? l1) l2)
        ((null? l2) l1)
        ((pred? (car l2) (car l1))
         (cons (car l2) (domerge pred? l1 (cdr l2))))
        (else (cons (car l1) (domerge pred? (cdr l1) l2))))))
  (set! merge-sort
    (lambda (pred? l)
      (if (null? l) l (dosort pred? l (length l)))))
  (set! merge
    (lambda (pred? l1 l2)
      (domerge pred? l1 l2))))


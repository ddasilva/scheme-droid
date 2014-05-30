;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  General intervals
;;   This module defines arithmetic on general intervals, which are finite
;;   unions of connected sets of reals. These connected sets can always be
;;   represented by intervals whose endpoints are in the extended reals.
;;
;;  Suggested exports: '(s->i s->S ppI ppS +S -S *S /S sqS negateS intersectS unionS union0S)
;;  described below:
;;
;;  The following procedures are defined
;;    CONSTRUCTORS:
;;      (s->i X)  ; convert string to an interval
;;      (s->S X)  ; convert a string to a general interval
;;    Pretty Printers:
;;      (ppI X)   ; pretty print an interval
;;      (ppS X)   ; pretty print a general interval
;;
;;    Interval Arithmetic Operators on general intervals:
;;      +S,-S,*S,/S,sqS,negateS,
;;    Binary Set operations on general intervals:
;;      intersectS,unionS,
;;      union0S  -- returns smallest connected interval containing its two arguments
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   PRIVATE procedures:
;;    lb ub lc uc
;;    (union0 X Y)
;;    +I -I *I /I  
;;    (unionI I1 I2 ... In)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This module uses the "ereal.scm" module which defines
;;    CONSTRUCTOR: string->ereal conversion
;;         (s->r S)  -- parses a string into an extended real
;;         (d->r S)  -- parses a string into a double
;;    CONSTANTS: infinite values, signed zeroes, signed units, and the NaN value
;;        +inf -inf -zero zero one -one NaN
;;    rounded arithmetic
;;        +lo +hi -lo -hi *lo *hi /lo /hi
;;    comparison operators:
;;        r>0 r<0 r<=0 r>=0 <r =r  minimum maximum
;;    miscellaneous ordering predicates
;;       isPosInf isNegInf isInfinite isFinite
;;       isPositive isNonNegative isZero isPosZero isNegZero isNonPositive isNegative
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-module "gia_eval/ereal.scm")
(define zz
  '(s->r d->r
    +inf -inf -zero zero one -one NaN 
    +lo +hi -lo -hi *lo *hi /lo /hi
    r>0 r<0 r<=0 r>=0 <r =r  minimum maximum
    isPosInf isNegInf isInfinite isFinite
    isPositive isNonNegative isZero isPosZero isNegZero isNonPositive isNegative))

(use-module "elf/basic.scm" 'import '(foldL))

(define EMPTY 'EMPTY)
(define ERROR 'ERROR)


;; (s->i x) --> parse the string x into an interval
(define (s->i x)
  (let ((LB (.indexOf x "["))
        (RB (.indexOf x "]"))
        (LP (.indexOf x "("))
        (RP (.indexOf x ")"))
        (CO (.indexOf x ",")))
   (if (and (= LB -1) (= LP -1))
     (let ((a (s->r x))) (if (isInfinite a) EMPTY (interval a  a #t #t)))
     (let ((a (s->r (.substring x (+ 1 (max LB LP)) CO)))
           (b (s->r (.substring x (+ 1 CO) (max RB RP)))))
      (interval
         a
         b
        (and (> LB -1) (not (equal? -inf a)))
        (and (> RB -1) (not (equal? +inf b))))))))

(define (->r x)
  (class-case (x)
    ((java.lang.String) (s->r x))
    ((java.lang.Number) (d->r (.doubleValue x)))
    ((java.math.BigDecimal) x)
    ((java.math.BigInteger) (java.math.BigDecimal. x))
    (else (throw {error: was expecting a number and found [x]\n}))))

(define (Icc x y)  (interval (->r x) (->r y) #t #t))
(define (Ico x y)  (interval (->r x) (->r y) #t #f))
(define (Ioc x y)  (interval (->r x) (->r y) #f #t))
(define (Ioo x y)  (interval (->r x) (->r y) #f #f))




;; (i x y) --> return the interval with bounds x and y
(define (i x y X Y)
  (cond ((<r y x) 'EMPTY)
        ((and (=r y x) (not (and X Y))) 'EMPTY)
        ((or (equal? x +inf) (equal? y -inf)) 'EMPTY)
        (else
         (list x y (and X (not (= x -inf))) (and Y (not (= y +inf)))))))

(define (fourth L) (list-ref L 3))
(define (fifth L) (list-ref L 4))

(define union (lambda R (cons 'union R)))

(define interval 
 (lambda R 
   (cons 'interval 
     (show (list "i" R) 
       (if (equal? (first R) 'EMPTY) 
           ()
           (apply i R))))))

;; This creates an interval but doesn't do any consistency checking....
(define (pseudointerval x y X Y)
   (list 'interval x y X Y))

(define (lb x) (if (pair? x) (let ((a (second x))) (if (isZero a) zero a)) x))
(define (ub x) (if (pair? x) (let ((a (third x))) (if (isZero a) -zero a)) x))
(define (lc x) (if (pair? x) (fourth x) #t))
(define (uc x) (if (pair? x) (fifth x) #t))


(define (sign x)
  (cond ((isPositive x) 1)
        ((isZero x) 0)
        ((isNegative x) -1)
        (else NaN)))

(define (iclass x)
  (case (sign (lb x))
    ((1)  (case (sign (ub x))
             ((1) 'p1)
             (else 'e)))
    ((0)  (case (sign (ub x))
             ((1) 'p0)
             ((0) 'z)
             (else 'e)))
    ((-1) (case (sign (ub x))
             ((1)  'm)
             ((0)  'n0)
             ((-1) 'n1)
             (else 'e)))
    (else 'e)))

(define (iclass0 x)
  (case (sign (lb x))
    ((1)  (case (sign (ub x))
             ((1) 'p)
             (else 'e)))
    ((0)  (case (sign (ub x))
             ((1) 'p)
             ((0) 'z)
             (else 'e)))
    ((-1) (case (sign (ub x))
             ((1)  'm)
             ((0)  'n)
             ((-1) 'n)
             (else 'e)))
    (else 'e)))


;; form the connected union of the two intervals x and y
;; It returns the smallest connected interval containing x and y.
;; This is the usual union if they have a non-empty intersection
;; the result is a single interval
(define (union0 x y)
 (define (nu a c A C)
   (or
     (and (<r a c) A)
     (and (=r a c)  (or A C))
     (and (<r c a) C)))
 (let
     ((a (lb x)) (A (lc x))
      (b (ub x)) (B (uc x))
      (c (lb y)) (C (lc y))
      (d (ub y)) (D (uc y)))
    (interval
     (minimum a c)
     (maximum b d)
     (nu a c A C)
     (nu b d D B))))

;; convert a general interval into a simiple interval
(define (S->I S) (foldL (rest (rest S)) union0 (first (rest S))))

(define (toInterval x)
   (if (pair? x) x
       (union (interval x x #t #t))))

;; this allows one to also construct empty infinite intervals
;; (-inf,-inf) and (inf,inf) which is used in iaeval
;; But this is a hack that should be rewritten. We could
;; then get rid of pseudointervals as well...

(define (toInterval0 x)
   (if (pair? x) x
   (if (isInfinite x) (union (pseudointerval x x #f #f))
       (union (interval x x #t #t)))))

(define (union0I x y)
  (union (union0 x y)))

;; form the intersection of the two intervals x and y
(define (intersect x y)
 (define (mu a c A C)
   (or
     (and (<r a c) C)
     (and (=r a c)  (and A C))
     (and (<r c a) A)))
 (let
     ((a (lb x)) (A (lc x))
      (b (ub x)) (B (uc x))
      (c (lb y)) (C (lc y))
      (d (ub y)) (D (uc y)))
  (let ((u (maximum a c))
        (v (minimum b d))
        (U (mu a c A C))
        (V (mu b d D B)))
   (union (interval u v U V)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arithmetic on general intervals, from HQvE paper
;; these always return a list of intervals 
;; (containing 0, 1, or 2 intervals)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define union (lambda R (cons 'union R)))
(define union-elts rest)


(define (+I x y)
 (union
  (if (or (equal? x 'EMPTY) (equal? y EMPTY)) 
      EMPTY 
      (let 
          ((a (lb x)) (A (lc x))
           (b (ub x)) (B (uc x))
           (c (lb y)) (C (lc y))
           (d (ub y)) (D (uc y)))
        (interval (+lo a c) (+hi b d) (and A C) (and B D))))))

(define (-I x y)
 (union
  (if (or (equal? x 'EMPTY) (equal? y EMPTY)) 
      EMPTY 
      (let
          ((a (lb x)) (A (lc x))
           (b (ub x)) (B (uc x))
           (c (lb y)) (C (lc y))
           (d (ub y)) (D (uc y)))
        (interval (-lo a d) (-hi b c) (and A D) (and B C))))))


(define (eta* A B) (let ((z (*r A B))) (and (isFinite z) (not (pair? z)))))


(define (sqI x)
  (let
     ((a (lb x)) (A (lc x))
      (b (ub x)) (B (uc x)))
   (union
     (case (iclass0 x)
        ((p) (interval (*lo a a) (*hi b b) A B))
        ((z) (interval zero zero #t #t))
        ((n) (interval (*lo b b) (*hi a a) B A))        
        ((m) (union0 (interval zero (*hi a a) #t A) (interval zero (*hi b b) #t B)))
        (else ERROR)))))

(define (*I x y)
  (define (psi1 a A c C)
    (and 
       (eta* a c) 
       (or
         (and A C) (and A (isZero a)) (and C (isZero c)))))

  (define (newint a A c C b B d D)
     (interval (*lo a c) (*hi b d) (psi1 a A c C) (psi1 b B d D)))

 (union
  (if (or (equal? x 'EMPTY) (equal? y EMPTY)) 
      EMPTY 
      (let
          ((a (lb x)) (A (lc x))
           (b (ub x)) (B (uc x))
           (c (lb y)) (C (lc y))
           (d (ub y)) (D (uc y)))
     (case (list (iclass0 x) (iclass0 y))
      (((n n)) (newint b B d D a A c C))
      (((n m)) (newint a A d D a A c C))
      (((n p)) (newint a A d D b B c C))
      (((m n)) (newint b B c C a A c C))
      (((m m)) (union0 
                  (newint a A d D b B d D) (newint b B c C a A c C)))
      (((m p)) (newint a A d D b B d D))
      (((p n)) (newint b B c C a A d D))
      (((p m)) (newint b B c C b B d D))
      (((p p)) (newint a A c C b B d D))
      (((z p)(z m)(z n)(p z)(m z)(n z)(z z))
               (interval zero      zero      #t             #t))
      (else    EMPTY))))))



(define (*I-optimized x y)
  (define (iand a A c C)
    (and (or A (equal? c zero)) (or C (equal? a zero)) (or A C) (eta* a c)))
 (union
  (if (or (equal? x 'EMPTY) (equal? y EMPTY)) 
      EMPTY 
      (let
          ((a (lb x)) (A (lc x))
           (b (ub x)) (B (uc x))
           (c (lb y)) (C (lc y))
           (d (ub y)) (D (uc y)))
     (case (list (iclass0 x) (iclass0 y))
      (((n n)) (interval (*lo b d) (*hi a c) (iand b B d D)       (iand a A c C)))
      (((n m)) (interval (*lo a d) (*hi a c) (and A D (eta* a d)) (and A C (eta* a c))))
      (((n p)) (interval (*lo a d) (*hi b c) (iand a A d D)       (iand b B c C)))

      (((m n)) (interval (*lo b c) (*hi a c) (and B C (eta* b c)) (and A C (eta* a c))))
      (((m m)) (union0 
               (interval (*lo a d) (*hi b d) (and A D (eta* a d)) (and B D (eta* b d)))
               (interval (*lo b c) (*hi a c) (and B C (eta* b c)) (and A C (eta* a c)))))
      (((m p)) (interval (*lo a d) (*hi b d) (and A D (eta* a d)) (and B D (eta* b d))))

      (((p n)) (interval (*lo b c) (*hi a d) (iand b B c C)       (iand a A d D)))
      (((p m)) (interval (*lo b c) (*hi b d) (and B C (eta* b c)) (and B D (eta* b d))))
      (((p p)) (interval (*lo a c) (*hi b d) (iand a A c C)       (iand b B d D)))

      (((z p)(z m)(z n)(p z)(m z)(n z)(z z))
               (interval zero      zero      #t             #t))
      (else    EMPTY))))))


;; this def has been moved into ereal.scm
;(define (eta/ A B) (let ((z (/r A B))) (and (isFinite z) (not (pair? z)))))

(define (/I x y)
    (define (psi1 a A c C)
    (and 
       (eta/ a c) 
       (or
         (and A C) (and A (isZero a)) (and C (isZero c)))))

  (define (newint a A d D b B c C )
     (interval (/lo a d) (/hi b c) (psi1 a A d D) (psi1 b B c C)))


  (if (or (equal? x 'EMPTY) (equal? y EMPTY)) 
      EMPTY 
      (let
          ((a (lb x)) (A (lc x))
           (b (ub x)) (B (uc x))
           (c (lb y)) (C (lc y))
           (d (ub y)) (D (uc y)))
     (case (list (iclass0 x) (iclass0 y))

      (((n n)) (union (newint b B c C a A d D)))
      (((m n)) (union (newint b B d D a A d D)))
      (((p n)) (union (newint b B d D a A c C)))
      (((n m)) (union 
                       (newint a A zero #f   b B     d D)    ;; <a,b>/<0,d>
                       (newint b B    c C    a A -zero #f))) ;; <a,b>/<c,-0>
      (((m m))
                (union (interval      -inf      +inf        #f       #f)))
      (((p m)) (union 
                       (newint b B -zero #f a A c C)       ;; <a,b>/<c,-0>
                       (newint a A d D b B zero #f)))      ;; <a,b>/<0,d>
      (((n p)) (union (newint a A c C b B d D)))
      (((m p)) (union (newint a A c C b B c C)))
      (((p p)) (union (newint a A d D b B c C)))

      (((z p)(z p1)(z m)(z n)(z n1))
                (union (interval      zero      zero           #t       #t)))
      (((p1 z)(p z)(m z)(n1 z)(n z)(z z)) 
                (union ))
      (else     ERROR)))))


(define (/I-optimized x y)
  (if (or (equal? x 'EMPTY) (equal? y EMPTY)) 
      EMPTY 
      (let
          ((a (lb x)) (A (lc x))
           (b (ub x)) (B (uc x))
           (c (lb y)) (C (lc y))
           (d (ub y)) (D (uc y)))
     (case (list (iclass x) (iclass0 y))

      (((n1 n)) (union (interval (/lo b c)  (/hi a d) (and B C (eta/ b c)) (and A D (eta/ a d)))) )
      (((n0 n)) (union (interval    zero    (/hi a d)  B                  (and A D (eta/ a d)))) )

      (((m n)) (union (interval (/lo b d)  (/hi a d) (and B D (eta/ b d))  (and A D (eta/ a d)))) )

      (((p0 n)) (union (interval (/lo b d)    zero    (and B D (eta/ b d))      A)))
      (((p1 n)) (union (interval (/lo b d)  (/hi a c) (and B D (eta/ b d)) (and A C (eta/ a c)))) )


      (((n1 m)) (union 
                       (interval      -inf (/hi b d)           #f         (and B D (eta/ b d)))
                       (interval (/lo b c) +inf      (and B C (eta/ b c))       #f)))
      (((n0 m)) (union 
                       (interval      -inf  zero           #f         B)
                       (interval      zero +inf            B         #f)))
      (((m m))
                (union (interval      -inf      +inf        #f       #f)))

      (((p0 m)) (union 
                       (interval      -inf  zero           #f         A)
                       (interval      zero +inf            A         #f)))
      (((p1 m)) (union 
                       (interval      -inf (/hi a c)           #f         (and A C (eta/ a c)))
                       (interval (/lo a d) +inf      (and A D (eta/ a d))       #f)))


      (((n1 p)) (union (interval (/lo a c)  (/hi b d) (and A C (eta/ a c)) (and B D (eta/ b d)))) )
      (((n0 p)) (union (interval (/lo a c)    zero    (and A C (eta/ a c)) B)))

      (((m p)) (union (interval (/lo a c)  (/hi b c) (and A C (eta/ a c))  (and B C (eta/ b c)))) )

      (((p0 p)) (union (interval    zero    (/hi b c)    A                (and B C (eta/ b c)))) )
      (((p1 p)) (union (interval (/lo a d)  (/hi b c) (and A D (eta/ a d)) (and B C (eta/ b c)))) )

      (((z p)(z p1)(z m)(z n)(z n1))
                (union (interval      zero      zero           #t       #t)))
      (((p1 z)(p z)(m z)(n1 z)(n z)(z z)) 
                (union ))
      (else     ERROR)))))


;; pretty print an exact general interval
(define (ppI x)
  (let
      ((a (lb x)) (A (lc x))
       (b (ub x)) (B (uc x)))
    (string-append
       (if A "[" "(")
       (pp-r a)
       ","
       (pp-r b)
       (if B "]" ")")
    )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arithmetic on sets of general intervals
;; this is a closed and total system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse a set of general intervals::
;;    >  (s->S "(-inf,-3]  [10,11) [799/7,801/7)")
;;    ((-Infinity (-3 1) #f #t) ((10 1) (11 1) #t #f) ((799 7) (801 7) #t #f))
(define (s->S S)
  (define (parse T)
     (if (not (.hasMoreTokens T)) ()
         (cons (s->i (.nextToken T)) (parse T))))
  (cons 'union (parse (java.util.StringTokenizer. S "{} " #f))))

;; pretty print a set of exact general intervals
(define (ppS S)
 (tryCatch
   (let ((S (union-elts S)))
     (if (equal? S ()) 
      "{}"
      (string-append
       "{"
       (ppI (first S))
       (apply string-append
          (map (lambda (x) (string-append "  " (ppI x))) (rest S)))
       "}")))
   (lambda(e) (.toString S))))



(define (unionS L)
  (apply unionI (apply append (map rest L))))

;; form the union of the set L of exact general intervals
;; the result is a minimal ordered list of exact general intervals
(define unionI  (lambda L
    ;; union the interval x into the preprocessed union y of intervals
    (define (union x y) 
;     (display (list 'union x y)) (newline)
     (if (null? y)
       (list x)
       (if (isLeft (first y) x)
           (cons (first y) (union x (rest y)))
           (if (isLeft x (first y))
               (cons x y)
               (union (union0 x (first y)) (rest y))))))
    ;; true if interval x is entirely to the left of interval y
    (define (isLeft x y)
       (let
         ((a (lb x)) (A (lc x))
          (b (ub x)) (B (uc x))
          (c (lb y)) (C (lc y))
          (d (ub y)) (D (uc y)))
      (or (<r b c)
          (and (=r b c) (not B) (not C)))))
    (define (union-iter L)
      (show (list 'union-iter L)
      (cond ((null? L) L)
            ((equal? (first L) (cons 'interval 'EMPTY)) (union-iter (rest L)))
            ((null? (cdr L)) L)
            (else 
               (union (first L) (union-iter (rest L)))))))
    (cons 'union (union-iter  L))))

(define (show a b) ; (display (list a b)) (newline)
   b)

(define (unionmap2 Op X Y)
  (apply unionI 
     (apply append (map (lambda(x) (apply append (map (lambda(y) (rest (Op x y))) (union-elts Y)))) (union-elts X)))))
(define (unionmap1 Op X)
  (apply unionI (apply append (map (lambda(x) (rest (Op x))) (union-elts X)))))

;; arithmetic operators on sets of intervals
(define (+S X Y) (unionmap2 +I X Y))
(define (-S X Y) (unionmap2 -I X Y))
(define (*S X Y) (unionmap2 *I X Y))
(define (/S X Y) (unionmap2 /I X Y))
(define (intersectS X Y) (unionmap2 intersect X Y))
(define (sqS X) (unionmap1 sqI X))
(define (negateS X) (-S (toInterval zero) X))

(define (union0S X Y)
  (apply unionI 
    (apply append 
      (map (lambda(x) (apply append (map (lambda(y) (rest (union0I x y))) (union-elts Y)))) (union-elts X)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ereal.scm
;;
;; Extended Reals implemented with BigDecimals
;;
;; This defines the following "public" procedures and constants
;;    (s->r S)  -- parses a string into an extended real
;;    +inf -inf -zero zero one -one NaN
;;    +lo +hi -lo -hi *lo *hi /lo /hi
;;    r>0 r<0 r<=0 r>=0 <r =r  minimum maximum
;;    isPosInf isNegInf isInfinite isFinite
;;    isPositive isNonNegative isZero isPosZero isNegZero isNonPositive isNegative
;;
;;  and uses the following "private" variables
;;    classify-r
;;    +r -r *r /r negate 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; when dividing two numbers, the number of digits of precision
;; is the maximum of the precisions of the operands and the constant "minscale"
(define minscale 10)

;; when multiplying two numbers, if the resulting precision exceeds maxscale,
;; then it is rounded down to maxscale
(define maxscale 10)


(define (d->r x) 
  (cond 
    ((or (.equals x  +inf) 
         (.equals x  -inf)
         (.equals x  -zero)) x)
    (else (java.math.BigDecimal. (.toString x)))))

;string->real
(define (s->r x)
  (cond 
    ((.equals x  "inf")  +inf)
    ((.equals x "+inf")  +inf)
    ((.equals x "-inf")  -inf)
    ((.equals x   "-0") -zero)
    (else (java.math.BigDecimal. x))))


;; pretty print an extended real
(define (pp-r x)
  (cond 
    ((equal? x +inf) "+inf")
    ((equal? x -inf) "-inf")
    ((equal? x -zero) "-0.0")
    ((equal? x  NaN)  "NaN")
    (else (.toString x))))


;; special values for reals
(define +inf java.lang.Double.POSITIVE_INFINITY$)
(define -inf java.lang.Double.NEGATIVE_INFINITY$)
(define -zero (/ 1 -inf))
(define zero (java.math.BigDecimal. "0.0"))
(define one (java.math.BigDecimal. "1.0"))
(define -one (java.math.BigDecimal. "-1.0"))
(define NaN java.lang.Double.NaN$)


;; tests on extended reals
(define (classify-r x)
  (cond 
    ((.equals +inf x)   3.0)
    ((.equals -inf x)  -3.0)
    ((.equals -zero x) -1.0)
    ((.isInstance java.math.BigDecimal.class x)
      (let ((zz (* 2.0 (.compareTo x zero))))
        (if (= zz 0) 1 (* 2.0 zz))))
    (else NaN)))

(define (isPosInf x)       (=  3.0 (classify-r x)))
(define (isNegInf x)       (= -3.0 (classify-r x)))
(define (isInfinite x)     (or   (isPosInf x) (isNegInf x)))
(define (isFinite x)       (not (isInfinite x)))
(define (isPositive x)     (>  (classify-r x) 1.0))
(define (isNonNegative x)  (>= (classify-r x) -1.0))
(define (isZero x)         (= (Math.abs (classify-r x)) 1))
(define (isPosZero x)      (=  1 (classify-r x))) 
(define (isNegZero x)      (= -1 (classify-r x))) 
(define (isNonPositive x)  (<= (classify-r x)  1.0))
(define (isNegative x)     (<= (classify-r x) -1.0))

(define r>0  isPositive)
(define r<0  isNegative)
(define r<=0 isNonPositive)
(define r>=0 isNonNegative)


(define (<r x y)
  (isPositive (-r y x)))

(define (=r x y)
  (or (equal? x y)
      (and (isZero x) (isZero y))
      (isZero (-r x y))))

(define (minimum x y)
  (if (<r x y) x y))

(define (maximum x y)
  (if (<r x y) y x))

(define (negate x)
  (cond 
        ((isPosInf x) -inf)
        ((isNegInf x) +inf)
        ((isNegZero x) zero)
        ((isPosZero x) -zero)
        (else  (.negate x))))
  
(define (+r x y)
  (cond 
        ((isPosInf x)
         (if (isNegInf y) NaN +inf))
        ((isNegInf x)
         (if (isPosInf y) NaN -inf))
        ((isInfinite y) y)
        ((or (isZero x) (isZero y))
         (if (isZero x) y x))
        (else 
          (let ((z (.add x y)))
             z))))

(define (-r x y)
  (cond 
        ((isPosInf x)
         (if (isPosInf y) NaN +inf))
        ((isNegInf x)
         (if (isNegInf y) NaN -inf))
        ((isInfinite y) (- y))
        ((or (isZero x) (isZero y))
         (if (isZero x) (negate y) x))
        (else 
          (let ((z (.subtract x y)))
            z))))

;; arithmetic operations that result in rounded results are
;; implemented as a pair (zlo zhi) representing an interval
;; that contains the result.

(define (exact R) (not (pair? R)))

(define (*r x y)
  (cond 
        ((isPosInf x)
         (cond ((isZero y)  NaN)
               ((isPositive y) +inf)
               ((isNegative y) -inf)
               (else NaN)))
        ((isNegInf x)
         (cond ((isZero y) NaN)
               ((isPositive y) -inf)
               ((isNegative y) +inf)
               (else NaN)))
        ((isZero x)
         (if (isInfinite y) NaN zero))
        ((isZero y)
         (if (isInfinite x) NaN zero))
        ((isInfinite y) 
         (cond ((isPositive x) y)
               ((isNegative x) (- y))
               (else NaN)))
        (else
          (let ((z (.multiply x y)))
            (if (<= (.scale z) maxscale)
                z
                (let (
                      (zlo (.setScale z maxscale java.math.BigDecimal.ROUND_FLOOR$))
                      (zhi (.setScale z maxscale java.math.BigDecimal.ROUND_CEILING$)))
                  (if (.equals zlo zhi) zlo (list zlo zhi))))))))



(define (eta* A B) 
   (let ((z (*r A B))) (and (isFinite z) (exact z))))



(define (/r x y)
  (cond 
        ((isPosZero x)
         (cond ((isZero y) NaN)
               ((isPositive y) zero)
               (else -zero)))
        ((isNegZero x)
         (cond ((isZero y) NaN)
               ((isPositive y) -zero)
               (else zero)))
        ((isPosZero y) 
         (if (isPositive x) +inf -inf))
        ((isNegZero y) 
         (if (isPositive x) -inf +inf))
        ((isPosInf x)
         (cond ((isInfinite y) NaN)
               ((isPositive y) +inf)
               ((isNegative y) -inf)
               (else NaN)))
        ((isNegInf x)
         (cond ((isInfinite y) NaN)
               ((isPositive y) -inf)
               ((isNegative y) +inf)
               (else NaN)))
        ((isPosInf y) 
           (if (isPositive x) zero -zero))
        ((isNegInf y) 
           (if (isPositive x) -zero zero))
        (else
         (let* ((xs (.scale x)) 
                (ys (.scale y))
                (zs (max minscale xs ys))
                (zlo (.divide x y zs java.math.BigDecimal.ROUND_FLOOR$))
                (zhi (.divide x y zs java.math.BigDecimal.ROUND_CEILING$)))
            (if (.equals zlo zhi) 
                zlo
               (list zlo zhi))))))

(define (eta/ A B) 
   (let ((z (/r A B))) (and (isFinite z) (exact z))))


(define (roundOp which op)
  (lambda (x y)
    (let ((z (op x y)))
      (if (pair? z) (which z) z))))

(define +lo (roundOp first +r))
(define +hi (roundOp second +r))
(define -lo (roundOp first -r))
(define -hi (roundOp second -r))
(define *lo (roundOp first *r))
(define *hi (roundOp second *r))
(define /lo (roundOp first /r))
(define /hi (roundOp second /r))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is a little demo of how to get the numeric tower from SISC
;; and use it in JScheme...
;; Use:
;; You must put sisc.jar on your classpath for this to work
;; Then all operators will use sisc numbers and you can convert
;; to Java numbers using .toLong .toDouble .toInt etc.
;;   (define (sleep-for-a-hundred-years)
;;      (Thread.sleep (.toLong (* 1000 60 60 24 100))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-module "elf/basic.scm" 'import '(foldL print describe))
;(load "elf/basic.scm")

(define (toMainNumber x)
  (class-case (x)
   ((byte) (.intValue x))
   ((short) (.intValue x))
   ((float) (.doubleValue x))
   (else x)
 ))

(define q 
  (lambda R
    (if (and (equal? (length R) 1) (.isInstance sisc.data.Quantity.class (first R) ))
        (first R)
        (apply sisc.data.Quantity.valueOf (map toMainNumber R))))) 


(define (toSiscMulOp op initialval)
 (lambda R  (foldL (map q R) op initialval)))

(define (toSiscUnaryOp op)
  (lambda (x) (apply op (list (q x)))))

(define (toSiscBinaryOp op)
  (lambda (x y) (apply op (list (q x) (q y)))))

(define resetops
 (lambda()
  (let ((oldops (list + * - / exp log)))
    (set! + (first oldops))
    (set! * (second oldops))
    (set! - (third oldops))
    (set! / (fourth oldops))
    (set! exp (sixth oldops))
    (set! log (seventh oldops)))))

(define + (toSiscMulOp .add (q 0)))
(define - (lambda R (if (equal? (length R) 1) (apply negate R) (apply (toSiscBinaryOp .sub) R))))
(define * (toSiscMulOp .mul (q 1)))
(define / (toSiscBinaryOp .div))
(define exp (toSiscUnaryOp .exp))
(define log (toSiscUnaryOp .log))


(define sin (toSiscUnaryOp .sin))
(define cos (toSiscUnaryOp .cos))
(define tan (toSiscUnaryOp .tan))
(define sinh (toSiscUnaryOp .sinh))
(define cosh (toSiscUnaryOp .cosh))
(define tanh (toSiscUnaryOp .tanh))
(define remainder (toSiscBinaryOp .remainder))

(define quotient (toSiscBinaryOp .quotient))

(define modulo (toSiscBinaryOp .modulo))

(define round (toSiscUnaryOp .exp))

(define lcm (toSiscBinaryOp .lcm))
(define gcd (toSiscBinaryOp .gcd))




(define truncate (toSiscUnaryOp .truncate))
(define ceiling (toSiscUnaryOp .ceiling))
(define floor (toSiscUnaryOp .floor))
(define (leftshift x n) (.lsh (q x) (.intValue n)))
(define (rightshift x n) (.rsh (q x) (.intValue n)))

(define negate (toSiscUnaryOp .negate))

(define sqrt (toSiscUnaryOp .sqrt))

(define toExact (toSiscUnaryOp .toExact))
(define toInexact (toSiscUnaryOp .toInexact))

(define realpart (toSiscUnaryOp .realpart))
(define imagpart (toSiscUnaryOp .imagpart))

(define numerator (toSiscUnaryOp .numerator))
(define denominator (toSiscUnaryOp .denominator))


(define < (toSiscBinaryOp .less))
(define > (toSiscBinaryOp .greater))


(define sisc-equal? (toSiscBinaryOp .equals))
(define sisc-eqv? (toSiscBinaryOp .eqv))
(define sisc-valueEqual? (toSiscBinaryOp .valueEqual))

(define (= x y) 
   (or 
     (equal? x y)
     (sisc-valueEqual? x y)
  ))

(define numeval
 (let ((js (jscheme.JScheme. (jsint.Scheme.currentEvaluator))))
   (lambda (x) (.eval js x))))

(define (<= x y) (or (< x y) (= x y)))
(define (>= x y) (or (> x y) (= x y)))


(define (sq x) (* x x))
(define (toNth x N)
 (if (< N 1) (q 1)
   (* x (toNth x (jsint.Op.sub N 1)))
 ))

(define-macro (catcherrors x)
 `(tryCatch ,x (lambda(e) e)))

;; Tests ...
(define (runtest)
  (print "*********************************")
  (print "(describe (q 1)) -->")
  (describe (q 1))
  (print "*********************************")
  
  (map print
   (list 
   "various ways of calculating the 100th power of 5"
   (toNth 5 100)    ; integers
   (toNth 5.0 100)  ; decimals
   (toNth (q "0+5i") 100) ;  gaussian integers
   (toNth (q "0+5.0i") 100) ; complex decimals
   (exp (* (log 5) 100))    ; exp and log
   (/ 1 (toNth (q "1/5") 100)) ; fractions
   (/ (exp (+ (* (log 5) 100) 50)) (exp 50))
   ; these don't behave as I expected....
   (catcherrors (/ (exp (+ (* (log 5) 100) 1000)) (exp 1000)))
   (catcherrors (/ 1 (toNth (q "0.2") 100)))
  ))
)


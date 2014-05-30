To: kanderson@hops.bbn.com
Subject: fannkuch.lisp

#||
;;; KRA 13NOV94: Times for fannkuch5.c on wheaton.
N  what   INSTR time (sec)
 9 gcc -O2 174  1.7
 9 cc -O2  171  1.8
 9 cc      294  5.9
 9 gcc     315  6.4
10 gcc -O2 174 20.6
10 cc -O2  171 22.1
10 cc      294 70.9
10 gcc     315 76.7



          Original Final Final Relative
N          9       9     10     9
--------  -------- ----  ----- --------
gcc -O2            1.70  20.60  1.00
cc -O2             1.80  22.10  1.07
Allegro   13.58    2.20  25.62  1.24 
CMU        8.49    2.36  27.22  1.32
Lispworks 15.29    2.74  32.56  1.58
Lucid      7.38    3.75  44.57  2.16
cc                 5.90  70.90  3.44
gcc                6.40  76.70  3.72

Lines of code:
gcc 117     
        fannkch fill-i shift  copy  flip   kreuz
Allegro 108      5/7   8/10    7/9  12/14
CMUCL   123      7/9   9/11    8/10 14/16  10/12
Lucid   139      6/8   9/11    8/10 13/15   8/8
||#

(defun fannkuch-0 (&optional (n (progn
                                (format *query-io* "n = ?")
                                (parse-integer (read-line *query-io*))
                                )          )  )
  ;; Original benchmark.
  (unless (and (> n 0) (<= n 100)) (return-from fannkuch-0))
  (let ((n n))
    (declare (fixnum n))
    (let ((perm (make-array n :element-type 'fixnum))
          (perm1 (make-array n :element-type 'fixnum))
          (zaehl (make-array n :element-type 'fixnum))
          (permmax (make-array n :element-type 'fixnum))
          (bishmax -1))
      (declare (type (simple-array fixnum (*)) perm perm1 zaehl permmax))
      (declare (fixnum bishmax))
      (dotimes (i n) (setf (svref perm1 i) i))
      (prog ((r n))
        (declare (fixnum r))
        Kreuz
          (when (= r 1) (go standardroutine))
          (setf (svref zaehl (- r 1)) r)
          (decf r)
          (go Kreuz)
        Dollar
          (when (= r n) (go fertig))
          (let ((perm0 (svref perm1 0)))
            (dotimes (i r) (setf (svref perm1 i) (svref perm1 (+ i 1))))
            (setf (svref perm1 r) perm0)
          )
          (when (plusp (decf (svref zaehl r))) (go Kreuz))
          (incf r)
          (go Dollar)
        standardroutine
          (dotimes (i n) (setf (svref perm i) (svref perm1 i)))
          (let ((Spiegelungsanzahl 0) (k 0))
            (declare (fixnum Spiegelungsanzahl k))
            (loop
              (when (= (setq k (svref perm 0)) 0) (return))
              (let ((k2 (ceiling k 2)))
                (declare (fixnum k2))
                (dotimes (i k2) (rotatef (svref perm i) (svref perm (- k i))))
              )
              (incf Spiegelungsanzahl)
            )
            (when (> Spiegelungsanzahl bishmax)
              (setq bishmax Spiegelungsanzahl)
              (dotimes (i n) (setf (svref permmax i) (svref perm1 i)))
          ) )
          (go Dollar)
        fertig
      )
      (format t "The maximum was ~D.~% at " bishmax)
      (format t "(")
      (dotimes (i n)
        (when (> i 0) (format t " "))
        (format t "~D" (+ (svref permmax i) 1))
      )
      (format t ")")
      (terpri)
      (values)
) ) )

(defun fannkuch-1 (&optional (n 10))
  ;; Driver for fannkuch-2
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
           (fixnum n))
  (unless (and (> n 0) (<= n 100)) (error "Bad N ~a" n))
  (let ((perm (make-array n :initial-element 0))
        (perm1 (make-array n :initial-element 0))
        (zaehl (make-array n :initial-element 0))
        (permmax (make-array n :initial-element 0))
        )
    (declare (simple-vector perm perm1 zaehl permmax)
             (dynamic-extent perm perm1 zaehl permmax))
    (let ((bishmax (fannkuch-2 n perm perm1 zaehl permmax)))
      (declare (fixnum bishmax))
      (format t "The maximum was ~D.~% at " bishmax)
      (format t "(")
      (dotimes (i n)
        (when (> i 0)
          (format t " "))
        (format t "~D" (the fixnum (1+ (the fixnum (svref permmax i))))))
      (format t ")"))
    (terpri)
    (values)))

(defun fannkuch-2 (n perm perm1 zaehl permmax)
  ;; Guts of benchmark.
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
           (type simple-vector perm perm1 zaehl permmax)
           (type (integer 1 100) n))
  (let ((bishmax -1))
    (declare (fixnum bishmax))
    (dotimes (i n)
      (declare (fixnum i))
      (setf (svref perm1 i) i))
    (prog ((r n))
       (declare (fixnum r))
       Kreuz
       (when (= r 1)
         (go standardroutine))
       (setf (svref zaehl (the fixnum (1- r))) r)
       (setf r (the fixnum (1- r)))
       (go Kreuz)
       Dollar
       (when (= r n)
         (go fertig))
       (let ((perm0 (svref perm1 0)))
         (declare (fixnum perm0))
         (dotimes (i r)
           (declare (fixnum i))
           (setf (svref perm1 i) (the fixnum (svref perm1 (the fixnum (1+ i))))))
         (setf (svref perm1 r) perm0))
       (when (> (the fixnum (setf (svref zaehl r)
                                  (the fixnum (1- (the fixnum (svref zaehl r))))))
                0)
         (go Kreuz))
       (setf r (the fixnum (1+ r)))
       (go Dollar)
       standardroutine
       (dotimes (i n)
         (declare (fixnum i))
         (setf (svref perm i) (the fixnum (svref perm1 i))))
       (let ((Spiegelungsanzahl 0)
             (k 0))
         (declare (fixnum Spiegelungsanzahl k))
         (loop
          (when (= (the fixnum (setq k (svref perm 0))) 0)
            (return))
           (let ((k2 (ash (the fixnum (1+ k)) -1)
                     #+ig (ceiling k 2)))
            (declare (fixnum k2))
            (dotimes (i k2)
              (declare (fixnum i))
              (rotatef (the fixnum (svref perm i))
                       (the fixnum (svref perm (the fixnum (- k i)))))))
          (setf Spiegelungsanzahl (the fixnum (1+ Spiegelungsanzahl))))
         (when (> Spiegelungsanzahl bishmax)
           (setq bishmax Spiegelungsanzahl)
           (dotimes (i n)
             (declare (fixnum i))
             (setf (svref permmax i) (the fixnum (svref perm1 i))))))
       (go Dollar)
       fertig)
    (values bishmax)))

(defvar counters (make-array 100 :initial-element 0))

(eval-when (compile)
  (defparameter n-counters 0)
  (defmacro counter ()
    (let ((result `(setf (svref counters ,n-counters)
                     (the fixnum
                       (1+ (the fixnum (svref counters ,n-counters)))))))
      (incf n-counters)
      result)))

(defun zero-counters ()
  (setq counters (make-array 100 :initial-element 0)))

(defun counter-report ()
  (labels
      ((factorial (n)
         (if (= n 0) 1 (* n (factorial (- n 1))))))
    (let ((f (/ 1.0 (factorial 10))))
      (dotimes (i n-counters)
        (format t "~%~3A ~9a ~6,2f" 
                i
                (svref counters i)
                (* f (svref counters i)))))))

#-lispworks
(eval-when (compile eval load)
  (deftype small () '(integer 0 101)))
#+lispworks
(eval-when (compile eval load)
  (deftype small () 'fixnum))
(defmacro small (a) `(the small ,a))
(defmacro s+ (a b) `(small (+ (small ,a) (small ,b))))
(defmacro s- (a b) `(small (- (small ,a) (small ,b))))
(defmacro s> (a b) `(> (small ,a) (small ,b)))
(defmacro s= (a b) `(= (small ,a) (small ,b)))
(defmacro sref (a i) `(small (svref ,a ,i)))
(defmacro setfs (a b) `(setf ,a (small ,b)))
(defmacro dotimess ((i n) &body body)
  `(dotimes (,i ,n)
     (declare (type small ,i))
     ,@body))

#||
USER(75): (fc 10)
; cpu time (non-gc) 61,650 msec (00:01:01.650) user, 0 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  61,650 msec (00:01:01.650) user, 0 msec system
; real time  63,003 msec (00:01:03.003)
; space allocation:
 0 cons cells,;  0 symbols, 416 other bytes
0   10          0.00
1   3628800     1.00
2   6235301     1.72
3   2943360     0.81
4   29433600    8.11
5   23201289    6.39
6   53459860   14.73
7   20          0.00
8   200         0.00
9   6235301     1.72
10  16099390    4.44
||#

(defun fannkuch-10 (n perm perm1 zaehl permmax)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
           (type simple-vector perm perm1 zaehl permmax)
           (type (integer 1 100) n))
  (dotimess (i n)
   (setfs (sref perm1 i) i))                   ; FILL-I
  (let ((bishmax -1)
        (r n))
    (loop                                      ; 1.00         
      (loop                                    ; 1.72 KREUZ
        (when (s= r 1) (return))
        (let ((i (s- r 1)))
          (setfs (sref zaehl i) r)
          (setq r i)))
      (when (not (or (zerop (sref perm1 0))
                     (let ((i (s- n 1)))
                       (s= (sref perm1 i) i))))
        (dotimesS  (i n)                       ; 0.81
         (setfs (sref perm i) (sref perm1 i))) ; 8.11 COPY
        (let ((Spiegelungsanzahl 0)
              (k 0))
          (loop                                ; 6.39 COUNT
            (when (s= (setq k (sref perm 0)) 0) (return))
            (let ((k2 (the small (ash (s+ k 1) -1))))
              (dotimesS (i k2)
               (let* ((temp (sref perm i))     ; 14.73 FLIP
                      (j (s- k i)))
                 (setfs (sref perm i) (sref perm j))
                 (setfs (sref perm j) temp))))
            (setq Spiegelungsanzahl (s+ Spiegelungsanzahl 1)))
          (when (s> Spiegelungsanzahl bishmax)
            (setq bishmax Spiegelungsanzahl)
            (dotimesS (i n)
             (setfs (sref permmax i)(svref perm1 i))))))
      (loop                                    ; 1.72
        (when (s= r n) (return-from fannkuch-10 bishmax))
        (let ((perm0 (sref perm1 0)))
          (let ((i 0))
            (loop                              ; 4.44 SHIFT
              (if (s= i r) (return))
              (let ((k (s+ i 1)))
                (setfs (sref perm1 i) (sref perm1 k))
                (setq i k))))
          (setfs (svref perm1 r) perm0))
        (when (s> (setfs (sref zaehl r) (s- (sref zaehl r) 1)) 0)
          (return))
        (setq r (s+ r 1))))))

;;; The following is to try to help Lucid's time by reducing the use of the
;;; stack.

(defun count-flips (n perm perm1)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0)))
  (dotimesS                             ; 0.81
   (i n)
   (setfs (sref perm i) (sref perm1 i)))
  (let ((Spiegelungsanzahl 0)
        (k 0))
    (loop                               ; 6.39 COUNT
     (when (s= (setq k (sref perm 0)) 0) (return))
     (let ((k2 (the small (ash (s+ k 1) -1))))
       (dotimesS 
        (i k2)
        (let* ((temp (sref perm i))     ; 14.73 FLIP
               (j (s- k i)))
          (setfs (sref perm i) (sref perm j))
          (setfs (sref perm j) temp))))
     (setq Spiegelungsanzahl (s+ Spiegelungsanzahl 1)))
    Spiegelungsanzahl))

(defun next-permutation (n perm1 zaehl)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0)))
  (let ((r 1))
    (loop                               ; 1.72
     (when (s= r n) (return-from next-permutation nil))
     (let ((perm0 (sref perm1 0)))
       (let ((i 0))
         (loop                          ; 4.44 SHIFT
          (if (s= i r) (return))
          (let ((k (s+ i 1)))
            (setfs (sref perm1 i) (sref perm1 k))
            (setq i k))))
       (setfs (svref perm1 r) perm0))
     (when (s> (setfs (sref zaehl r) (s- (sref zaehl r) 1)) 0)
       (return t))
     (setq r (s+ r 1)))
    (loop
     (when (s= r 1) (return-from next-permutation t))
     (let ((i (s- r 1)))
       (setfs (sref zaehl i) r)
       (setq r i)))))

;;; 35.74 Sec with count-flips.
;;; 36.92 Sec with next-permutation-added.
(defun fannkuch-11 (n perm perm1 zaehl permmax)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
           (type simple-vector perm perm1 zaehl permmax)
           (type (integer 1 100) n))
  (dotimess
   (i n)
   (setfs (sref perm1 i) i))            ; FILL-I
  (let ((r n))
    (loop                               ; 1.72 KREUZ
     (when (s= r 1) (return))
     (let ((i (s- r 1)))
       (setfs (sref zaehl i) r)
       (setq r i))))
  (let ((bishmax -1))
    (loop                               ; 1.00         
     (when (not (or (zerop (sref perm1 0))
                    (let ((i (s- n 1)))
                      (s= (sref perm1 i) i))))
       (let ((Spiegelungsanzahl (count-flips n perm perm1)))
         (when (s> Spiegelungsanzahl bishmax)
           (setq bishmax Spiegelungsanzahl)
           (dotimesS 
            (i n)
            (setfs (sref permmax i)(svref perm1 i))))))
     (if (not (next-permutation n perm1 zaehl))
         (return-from fannkuch-11 bishmax))
     )))

(let ((a (make-array 10))
      (b (make-array 10))
      (c (make-array 10))
      (d (make-array 10)))
  (defun f (n)
    (time 
     (fannkuch-10 n a b c d))))
         
#+counting
(let ((a (make-array 10))
      (b (make-array 10))
      (c (make-array 10))
      (d (make-array 10)))
  (defun fc (n)
    (time 
     (fannkuch-10c n a b c d))
    (counter-report)))

(defun fannkuch-benchmark ()
  ;; In GCL you can't disassemble compiled code, only interpreted code.
  #-gcl
  (dolist (f '(fill-i shift svcopy flip kreuz-loop fannkuch-10))
    (print f)
    (disassemble f))
  (let ((perm (make-array 100))
        (perm1 (make-array 100))
        (zaehl (make-array 100))
        (permmax (make-array 100)))
    ;; This is too slow on my powerbook.
    #-mcl (dotimes (i 5) (time (fannkuch-0 9)))
    (dotimes (i 5) (time (fannkuch-10 9 perm perm1 zaehl permmax)))
    (dotimes (i 5) (time (fannkuch-11 10 perm perm1 zaehl permmax)))
    #+old (dotimes (i 5) (time (fannkuch-2 10 perm perm1 zaehl permmax)))
    #-mcl (dotimes (i 5) (time (fannkuch-10 10 perm perm1 zaehl permmax)))
    #-mcl (dotimes (i 5) (time (fannkuch-11 10 perm perm1 zaehl permmax)))
    ))


(defun fill-i (perm1 N)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
           (type simple-vector perm1)
           (fixnum N))
  (dotimess (i n)
    (setfs (sref perm1 i) i))
  (values))

(defun shift (perm1 r)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
           (simple-vector perm1)
           (fixnum r))
  (let ((perm0 (sref perm1 0)))
    (let ((i 0))
      (loop                                    ; 4.44 SHIFT
        (if (s= i r) (return))
        (let ((k (s+ i 1)))
          (setfs (sref perm1 i) (sref perm1 k))
          (setq i k))))
    (setfs (svref perm1 r) perm0))
  (values))

(defun Kreuz-loop (zaehl r)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
           (simple-vector zaehl)
           (fixnum r))
  (loop                                        ; 1.72
    (when (s= r 1) (return))
    (let ((i (s- r 1)))
      (setf (sref zaehl i) r)
      (setf r i)))
  (values))

(defun svcopy (perm perm1 N)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
           (type simple-vector perm perm1)
           (fixnum N))
  (dotimess (i n)
    (setf (svref perm i) (the fixnum (svref perm1 i))))
  (values))

(defun flip (perm k k2)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
           (simple-vector perm)
           (fixnum k k2))
  (dotimesS 
   (i k2)
   (let* ((temp (sref perm i))  ; 14.73 FLIP
          (j (s- k i)))
     (setf (sref perm i) (sref perm j)
           (sref perm j) temp)))
  (values))

;; (fannkuch-benchmark)

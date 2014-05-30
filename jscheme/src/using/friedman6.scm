(define even?/odd?-procs
  (vector
    (lambda (procs n)
      (if (zero? n) #t
        ((vector-ref procs 1) procs (- n 1))))
    (lambda (procs n)
      (if (zero? n) #f
        ((vector-ref procs 0) procs (- n 1))))))

(define test-even?/odd?-procs
  (lambda ()
    (equal?
      ((vector-ref even?/odd?-procs 0) even?/odd?-procs 5)
      #f)))

(define enumerate-env
  (lambda (vars)
    (let loop ((vars vars) (i 0))
      (cond
        ((null? vars) '())
        (else (cons `(,(car vars) ,i)
                (loop (cdr vars) (+ i 1))))))))

(define append-env
  (lambda (e p)
    (cond
      ((null? e) p)
      (else
    (append e
          (let ((m (+ (cadar (last-pair e)) 1)))
            (map (lambda (pr)
                   `(,(car pr) ,(+ (cadr pr) m)))
              p)))))))

(define p '(a b c))

(define test-enumerate-env
  (lambda ()
    (equal? (enumerate-env p) '((a 0) (b 1) (c 2)))))

(define trim-env
  (lambda (e)
    (cond
      ((null? e) '())
      ((assv (caar e) (cdr e)) (trim-env (cdr e)))
      (else (cons (car e) (trim-env (cdr e)))))))

(define append-env append)

(define append-env* (enumerate-env (append-env p '(a d c))))

(define test-env-append-env
  (lambda ()
    (and
      (equal? append-env* '((a 0) (b 1) (c 2) (a 3) (d 4) (c 5)))
      (equal? (trim-env append-env*) '((b 1) (a 3) (d 4) (c 5)))
      (equal? (let* ((a 0) (b 1) (c 2) (a 3) (d 4) (c 5))
                (list a b c d))
        '(3 1 5 4)))))

(write (map (lambda (t) (t))
         (list test-even?/odd?-procs test-enumerate-env test-env-append-env)))
(newline)

(define (_fx c e) (append-env (car c) e))
(define (_mx c e) (append-env (cadr c) e))
(define (_mp oc p) (vector-ref (caddr oc) p))
(define (_mp! oc p v) (vector-set! (car oc) p v))
(define (_fp o p) (vector-ref (car o) p))
(define (_fp! o p v) (vector-set! (car o) p v))
(define (_mteq? oc1 oc2) (eq? (cdr oc1) (cdr oc2)))

(define (_n c)
  (cons (make-vector (length (car c))) (cdr c)))

(define (_mv oc m)
  (_mp oc
    (let loop ((m* (cadr oc)) (pos 0))
      (if (eqv? (car m*) m)
        pos
        (loop (cdr m*) (+ pos 1))))))

(define <o>
  (list
    '()
    '(isa? init)
    (vector
      (lambda (this c)
        (_mteq? this c))
      (lambda (this . args)
        (void)))))

(define <p>
  (list
    (_fx <o> '(x y))
    (_mx <o> '(move get-loc diag))
    (vector
      (lambda (this c)
        (or (_mteq? this c)
          ((_mp <o> 0) <o> c)))
      (lambda (this x^ y^)
        (_fp! this 0 x^)
        (_fp! this 1 y^)
        ((_mp <o> 1) this))
      (lambda (this dx dy)
        (_fp! this 0 (+ (_fp this 0) dx))
        (_fp! this 1 (+ (_fp this 1) dy)))
      (lambda (this)
        (list (_fp this 0) (_fp this 1)))
      (lambda (this a)
        ((_mp this 2) this a a)))))

(define <cp>
  (list
    (_fx <p> '(hue))
    (_mx <p> '(get-hue diag&set))
    (vector
      (lambda (this c)
        (or (_mteq? this c)
         ((_mp <p> 0) <p> c)))
      (lambda (this x^ y^ hue^)
        (_fp! this 2 hue^)
        ((_mp <p> 1) this x^ y^))
      (_mp <p> 2)
      (_mp <p> 3)
      (_mp <p> 4)
      (lambda (this) (_fp this 2))
      (lambda (this a)
        ((_mp this 4) this a)
        (_fp! this 2 a)))))

(define <scp>
  (list
    (_fx <cp> '(y))
    (_mx <cp> '(show-y))
    (vector
      (lambda (this c)
        (or (_mteq? this c)
            ((_mp <cp> 0) <cp> c)))
      (lambda (this x^ y^ hue^)
        (_fp! this 3 ": Stuck: ")
        ((_mp <cp> 1) this x^ y^ hue^))
      (lambda (this x^ y^)
        ((_mp this 7) this))
      (_mp <cp> 3)
      (lambda (this a)
        (write (_fp this 2))
        ((_mp <cp> 4) this a))
      (_mp <cp> 5)
      (_mp <cp> 6)
      (lambda (this)
        (display (_fp this 3))))))

(define map-nullary-method
  (lambda (oc m*)
    (map (lambda (m) ((_mv oc m) oc)) m*)))

(define test-<p>
  (lambda ()
    (let ((p (_n <p>)))
      ((_mp p 1) p 12 13)  ;;; show the class after it is initialize, too.
                           ;;; but at top level.
      ((_mv p 'move) p 14 15)
      (list
        (map-nullary-method p '(get-loc))
        ((_mp p 0) p <p>)))))

(define test-<cp>
  (lambda ()
    (let ((cp (_n <cp>))
          (p (_n <p>)))
      ((_mp cp 1) cp 16 17 7)
      ((_mv cp 'diag&set) cp 8)
      (list
        (map-nullary-method cp '(get-loc get-hue))
        ((_mp p 0) p <cp>)
        ((_mp cp 0) cp <p>)))))

(define test-<scp>
  (lambda ()
    (let ((scp (_n <scp>))
          (p (_n <p>)))
      ((_mp scp 1) scp 18 19 9)
      ((_mv scp 'diag&set) scp 10)
      (list
        (map-nullary-method scp '(get-loc get-hue))
        ((_mp p 0) p <scp>)
        ((_mp scp 0) scp <p>)))))

(define test
  (lambda (<cp>^)
    (let ((p (_n <p>))
          (cp (_n <cp>^)))
      ((_mp cp 1) cp 18 19 9)
      ((_mv cp 'diag&set) cp 10)
      (list
        (map-nullary-method cp '(get-loc get-hue))
        ((_mp p 0) p <cp>^)
        ((_mp cp 0) cp <p>)))))
        
(pretty-print (test-<p>))
(pretty-print (test <cp>))
(pretty-print (test <scp>))

;;; ((26 28))
;;; ((103 -70) 87)
;;; 88: Stuck: (((18 19) 89) #f #t)

(define <scp>  ;;; Introducing super
  (let ((super <cp>))
    (list
      (_fx super '(y))
      (_mx super '(show-y))
      (vector
        (lambda (this c)
          (or (_mteq? this c)
              ((_mp super 0) super c)))
        (lambda (this x^ y^ hue^)
          (_fp! this 3 ": Stuck: ")
          ((_mp super 1) this x^ y^ hue^))
        (lambda (this x^ y^)
          ((_mp this 7) this))
        (_mp super 3)
        (lambda (this a)
          (write (_fp this 2))
          ((_mp super 4) this a))
        (_mp super 5)
        (_mp super 6)
        (lambda (this)
          (display (_fp this 3)))))))

(pretty-print (test <scp>))

(define <scp>  ;;; Position variables for methods
  (let ((isa? 0)
        (init 1)
        (move 2)
        (get-loc 3)
        (diag 4)
        (get-hue 5)
        (diag&set 6)
        (show-y 7))
    (let ((super <cp>))
      (list
        (_fx super '(y))
        (_mx super '(show-y))
        (vector
          (lambda (this c)
            (or (_mteq? this c)
              ((_mp super isa?) super c)))
          (lambda (this x^ y^ hue^)
            (_fp! this 3 ": Stuck: ")
            ((_mp super init) this x^ y^ hue^))
          (lambda (this x^ y^)
            ((_mp this 7) this))
          (_mp super get-loc)
          (lambda (this a)
            (write (_fp this 2))
            ((_mp super diag) this a))
          (_mp super get-hue)
          (_mp super diag&set)
          (lambda (this)
            (display (_fp this 3))))))))

(pretty-print (test <scp>))

(define <scp>  ;;; Position variables for fields
  (let* ((x 0) (y 1) (hue 2) (y 3))
    (let ((isa? 0)
          (init 1)
          (move 2)
          (get-loc 3)
          (diag 4)
          (get-hue 5)
          (diag&set 6)
          (show-y 7))
      (let ((super <cp>))
        (list
          (_fx super '(y))
          (_mx super '(show-y))
          (vector
            (lambda (this c)
              (or (_mteq? this c)
                ((_mp super isa?) super c)))
            (lambda (this x^ y^ hue^)
              (_fp! this y ": Stuck: ")
              ((_mp super init) this x^ y^ hue^))
            (lambda (this x^ y^)
              ((_mp this show-y) this))
            (_mp super get-loc)
            (lambda (this a)
              (write (_fp this hue))
              ((_mp super diag) this a))
            (_mp super get-hue)
            (_mp super diag&set)
            (lambda (this)
              (display (_fp this y)))))))))

(pretty-print (test <scp>))

(define <scp>   ;;; Naive Lifting
  (let* ((x 0) (y 1) (hue 2) (y 3))
    (let ((isa? 0)
          (init 1)
          (move 2)
          (get-loc 3)
          (diag 4)
          (get-hue 5)
          (diag&set 6)
          (show-y 7))
      (let ((super <cp>))
        (let ((isa?
                (lambda (this c)
                  (or (_mteq? this c)
                    ((_mp super isa?) super c))))
              (init
                (lambda (this x^ y^ hue^)
                  (_fp! this y ": Stuck: ")
                  ((_mp super init) this x^ y^ hue^)))
              (move
                (lambda (this x^ y^)
                  ((_mp this show-y) this)))
              (get-loc (_mp super get-loc))
              (diag
                (lambda (this a)
                  (write (_fp this hue))
                  ((_mp super diag) this a)))
              (get-hue (_mp super get-hue))
              (diag&set (_mp super diag&set))
              (show-y (lambda (this)
                        (display (_fp this y)))))
          (list
            (_fx super '(y))
            (_mx super '(show-y))
            (vector
              isa?
              init
              move
              get-loc
              diag
              get-hue
              diag&set
              show-y)))))))

(pretty-print (test <scp>))

(define <scp>  ;;; Triply-nested let
  (let* ((x 0) (y 1) (hue 2) (y 3))
    (let ((isa? 0)
          (init 1)
          (move 2)
          (get-loc 3)
          (diag 4)
          (get-hue 5)
          (diag&set 6)
          (show-y 7))
      (let ((super <cp>))
        (let ((g1
                (lambda (this c)
                  (or (_mteq? this c)
                      ((_mp super isa?) super c))))
              (g2
                (lambda (this x^ y^ hue^)
                  (_fp! this y ": Stuck: ")
                  ((_mp super init) this x^ y^ hue^)))
              (g3
                (lambda (this x^ y^)
                  ((_mp this show-y) this)))
              (g4
                (lambda (this a)
                  (write (_fp this hue))
                  ((_mp super diag) this a)))
              (g5 (lambda (this) (display (_fp this y)))))
          (let ((isa? (_mp super isa?))
                (init (_mp super init))
                (move (_mp super move))
                (get-loc (_mp super get-loc))
                (diag (_mp super diag))
                (get-hue (_mp super get-hue))
                (diag&set (_mp super diag&set)))
            (let ((isa? g1)
                  (init g2)
                  (move g3)
                  (diag g4)
                  (show-y g5))
              (list
                (_fx super '(y))
                (_mx super '(show-y))
                (vector
                  isa?
                  init
                  move
                  get-loc
                  diag
                  get-hue
                  diag&set
                  show-y)))))))))

(pretty-print (test <scp>))

(define <scp>  ;;; Quadruply-nested let
  (let* ((x 0) (y 1) (hue 2) (y 3))
    (let ((isa? 0)
          (init 1)
          (move 2)
          (get-loc 3)
          (diag 4)
          (get-hue 5)
          (diag&set 6)
          (show-y 7))
      (let ((super <cp>))
        (let ((h0 (_mp super isa?))
              (h1 (_mp super init))
              (h2 (_mp super move))
              (h3 (_mp super get-loc))
              (h4 (_mp super diag))
              (h5 (_mp super get-hue))
              (h6 (_mp super diag&set)))
          (let ((g1
                  (lambda (this c)
                    (or (_mteq? this c)
                      ((_mp super isa?) super c))))
                (g2
                  (lambda (this x^ y^ hue^)
                    (_fp! this y ": Stuck: ")
                    ((_mp super init) this x^ y^ hue^)))
                (g3
                  (lambda (this x^ y^)
                    ((_mp this show-y) this)))
                (g4
                  (lambda (this a)
                    (write (_fp this hue))
                    ((_mp super diag) this a)))
                (g5 (lambda (this) (display (_fp this y)))))
            (let ((isa? (_mp super isa?))
                  (init (_mp super init))
                  (move (_mp super move))
                  (diag (_mp super diag))
                  (get-loc (_mp super get-loc))
                  (get-hue (_mp super get-hue))
                  (diag&set (_mp super diag&set)))
              (let ((isa? g1)
                    (init g2)
                    (move g3)
                    (diag g4)
                    (show-y g5))
                (list
                  (_fx super '(y))
                  (_mx super '(show-y))
                  (vector
                    isa?
                    init
                    move
                    get-loc
                    diag
                    get-hue
                    diag&set
                    show-y))))))))))

(pretty-print (test <scp>))

;; Start of Figure 1

(define-syntax with-implicit
  (syntax-rules ()
    ((_ (ctx id ...) body0 body1 ...)
     (with-syntax ((id (datum->syntax-object (syntax ctx) 'id)) ...)
       body0 body1 ...))))

(define-syntax extender
  (lambda (x)
    (syntax-case x ()
      ((_ ctx ((s k) ...) (all-f ...) ((f j) ...) ((m i) ...)
         ((m-var g e) ...))
       (with-syntax (((h ...) (generate-temporaries (syntax (s ...)))))
         (with-implicit (ctx super)
           (syntax (let ((f j) ...)
               (let ((m i) ...)
                 (lambda (super)
                   (let ((h (_mp super k)) ...)
                     (let ((g e) ...)
                       (let ((s h) ...)
                         (let ((m-var g) ...)
                           (list '(all-f ...)
                                 '(m ...)
                                 (vector m ...))))))))))))))))

(define-syntax assv-macro
  (lambda (x)
    (syntax-case x ()
      ((_ i ((k0 h0) (k1 h1) ...))
       (if (eqv? (syntax-object->datum (syntax k0)) (syntax-object->datum (syntax i)))
           (syntax h0)
           (syntax (assv-macro i ((k1 h1) ...))))))))

(define-syntax build-shadow
  (lambda (x)
    (syntax-case x ()
      ((_ ctx sup-f sup-m (f-var ...) ((m-var g e) ...))
       (let ((sup-f (syntax-object->datum (syntax sup-f)))
             (sup-m (syntax-object->datum (syntax sup-m)))
             (f-vars (syntax-object->datum (syntax (f-var ...))))
             (m-vars (syntax-object->datum (syntax (m-var ...)))))
         (let ((f (append-env sup-f f-vars))
               (m (append-env sup-m (fresh-m-vars m-vars sup-m))))
           (with-syntax
             ((((s k) ...) (datum->syntax-object (syntax ctx) (enumerate-env sup-m)))
              (((m i) ...) (datum->syntax-object (syntax ctx) (enumerate-env m)))
              (((f j) ...) (datum->syntax-object (syntax ctx) (trim-env (enumerate-env f))))
              ((all-f ...) (datum->syntax-object (syntax ctx) f)))
             (syntax (lambda (xx)
                 (syntax-case xx ()
                   ((__)
                    (syntax (extender ctx ((s k) ...) (all-f ...) ((f j) ...) ((m i) ...) ((m-var g e) ...))))
                   ((__ an-m-var oc) (syntax (_mp oc (assv-macro an-m-var ((m i) ...)))))
                   ((__ ctx (f-var^ (... ...)) ((m-var^ e^) (... ...)))
                    (with-syntax (((g^ (... ...)) (generate-temporaries (syntax (m-var^ (... ...))))))
                      (syntax (build-shadow ctx (all-f ...) (m ...)
                          (f-var^ (... ...))
                          ((m-var^ g^ e^) (... ...))))))))))))))))

(define fresh-m-vars
  (lambda (m-vars sup-m-vars)
    (cond
      ((null? m-vars) '())
      ((memv (car m-vars) sup-m-vars)
       (fresh-m-vars (cdr m-vars) sup-m-vars))
      (else (cons (car m-vars)
              (fresh-m-vars (cdr m-vars) sup-m-vars))))))

;;; End of Figure 1

;;; Macro from page 10.
(define-syntax extend-shadow
  (lambda (x)
    (syntax-case x ()
      ((_ sup-shadow (f-var ...) ((m-var e) ...))
       (with-implicit (_ super isa?)
         (syntax (sup-shadow _ (f-var ...)
             ((isa? (lambda (this c)
                      (or (_mteq? this c)
                        ((_mp super 0) super c))))
              (m-var e) ...))))))))

;;; Start macros from page 11.
(define-syntax create-class
  (syntax-rules ()
    ((_ host-shadow super-class)
     ((host-shadow) super-class))))

(define-syntax build-<<o>>
  (lambda (x)
    (syntax-case x ()
      ((_ ((m e) ...))
       (with-syntax
           (((g ...) (generate-temporaries (syntax (m ...)))))
         (syntax (build-shadow _ () () ()
             ((m g e) ...))))))))

(define-syntax <<o>>
  (build-<<o>>
    ((isa? (lambda (this c) (_mteq? this c)))
     (init (lambda (this . args) (void))))))

(define <o> (create-class <<o>> #f))
;;; End macros from page 11
(define-syntax <<p>>
  (extend-shadow <<o>> (x y)
    ((init
       (lambda (this x^ y^)
         (_fp! this x x^)
         (_fp! this y y^)
         ((_mp super init) this)))
     (move
       (lambda (this dx dy)
         (_fp! this x (+ (_fp this x) dx))
         (_fp! this y (+ (_fp this y) dy))))
     (diag
       (lambda (this a)
         ((_mp this move) this a a)))
     (get-loc
       (lambda (this)
         (list (_fp this x) (_fp this y)))))))

(define <p> (create-class <<p>> <o>))

(define-syntax <<cp>>
  (extend-shadow <<p>> (hue)
    ((init
       (lambda (this x^ y^ hue^)
         (_fp! this hue hue^)
         ((_mp super init) this x^ y^)))
     (get-hue
       (lambda (this)
         (_fp this hue)))
     (diag&set
       (lambda (this a)
         ((_mp this diag) this a)
         (_fp! this hue a))))))

(define <cp> (create-class <<cp>> <p>))

(define-syntax <<scp>>
  (extend-shadow <<cp>> (y)
    ((init
       (lambda (this x^ y^ hue^)
         (_fp! this y ": Stuck: ")
         ((_mp super init) this x^ y^ hue^)))
     (move
       (lambda (this x^ y^)
         ((_mp this show-y) this)))
     (diag
       (lambda (this a)
         (write (_fp this hue))
         ((_mp super diag) this a)))
     (show-y
       (lambda (this)
         (display (_fp this y)))))))

(define <scp> (create-class <<scp>> <cp>))

(pretty-print (test <scp>))

;;; Start of Figure 2

(define-syntax if-shadowed
  (lambda (x)
    (syntax-case x ()
      ((_ id ctx conseq altern)
       (if (not
             (free-identifier=? (syntax id)
               (datum->syntax-object (syntax ctx)
                 (syntax-object->datum (syntax id)))))
           (syntax conseq)
           (syntax altern))))))

(define-syntax field-var
  (lambda (x)
    (syntax-case x ()
      ((_ ctx id this j)
       (syntax (identifier-syntax
           (var
             (if-shadowed id ctx id (_fp this j)))
           ((set! var val)
            (if-shadowed id ctx (set! id val) (_fp! this j val)))))))))

(define-syntax method-var
  (lambda (x)
    (syntax-case x ()
      ((_ ctx mapping m super this i)
       (syntax (lambda (x)
           (syntax-case x (super)
             ((m_ super arg (... ...))
              (syntax (if-shadowed m ctx (m super arg (... ...)) ((assv-macro i mapping) this arg (... ...)))))
             ((m_ oc arg (... ...))
              (syntax (if-shadowed m ctx (m oc arg (... ...)) (let ((oc^ oc)) ((_mp oc^ i) oc^ arg (... ...))))))
             ((m_)
              (syntax (if-shadowed m ctx (m) (error 'method "Cannot take zero arguments:" m))))
             (m_ (identifier? (syntax m_))
              (syntax (if-shadowed m ctx m (error 'method "Cannot be a symbol:" m)))))))))))

(define-syntax extender
  (lambda (syn)
    (syntax-case syn ()
      ((_ ctx ((s k) ...) (all-f ...) ((f j) ...) ((m i) ...) ((m-var g e) ...))
       (with-syntax (((h ...) (generate-temporaries (syntax (s ...)))))
         (with-implicit (ctx super method)
           (syntax (lambda (super)
               (let ((h (_mp super k)) ...)
                 (let-syntax
                     ((transf-body
                        (lambda (xx)
                          (syntax-case xx ()
                            ((_ __ ctx body0 body1 (... ...))
                             (with-implicit (__ this super set! f ... m ...)
                               (syntax (let-syntax ((f (field-var ctx f this j)) ...)
                                   (let-syntax ((m (method-var ctx ((k h) ...) m super this i)) ...)
                                     body0 body1 (... ...))))))))))
                   (let-syntax
                       ((method
                          (lambda (xx)
                            (syntax-case xx ()
                              ((__ params body0 body1 (... ...))
                               (with-implicit (__ this)
                                 (syntax (lambda (this . params)
                                     (transf-body __ ctx body0 body1 (... ...))))))))))
                     (let ((g e) ...)
                       (let ((s h) ...)
                         (let ((m-var g) ...)
                           (list '(all-f ...)
                             '(m ...)
                             (vector m ...))))))))))))))))

;;; End of Figure 2

(define-syntax <<scp>>
  (extend-shadow <<cp>> (y)
    ((init
       (method (x^ y^ hue^)
         (set! y ": Stuck: ")
         (init super x^ y^ hue^)))
     (move
       (method (x^ y^)
         (show-y this)))
     (diag
       (method (a)
         (write hue)
         (diag super a)))
     (show-y
       (method () (display y))))))

(print-gensym #f)

(pretty-print (expand '(create-class <<scp>> <cp>)))

(define <scp> (create-class <<scp>> <cp>))

(pretty-print (test <scp>))

;;; Begin Section 8.4
;;; First Example
(define-syntax <<escp>>
  (extend-shadow <<scp>> ()
    ((init
       (method (x^ y^ hue)
         (display hue)
         (init super x^ y^ hue)))
     (show-y
       (let ((hue "outside ")
             (diag (lambda (x y) ;;; loops if diag*
                      (display "moving "))))
         (method ()
           (display hue)
           (diag 5 5) ;;; loops if diag*
           (let ((hue "inside ")
                 (diag (lambda (n self)
                         (diag self n))))
             (display hue)
             (diag 5 this))))))))

(define <escp> (create-class <<escp>> <scp>))
(pretty-print (test <escp>))

;;; Second Example
(define <escp>-maker
  (lambda (x)
    (let-syntax
        ((<<escp>>
           (extend-shadow <<scp>> ()
             ((e (begin
                   (write 1)
                   (let ((y 1))
                     (method (q r . args)
                       (+ x y q r (car args))))))))))
      (lambda (s)
        (create-class <<escp>> s)))))

(pretty-print
  (expand
    '(lambda (x)
    (let-syntax
        ((<<escp>>
           (extend-shadow <<scp>> ()
             ((e (begin
                   (write 1)
                   (let ((y 1))
                     (method (q r . args)
                       (+ x y q r (car args))))))))))
      (lambda (s)
        (create-class <<escp>> s))))))

(pretty-print   ;;; an additional test
  (expand '(lambda (x)
             (let-syntax
                 ((<<escp>>
                    (extend-shadow <<scp>> (x)
                      ((init (method (x^ x* y* hue*)
                               (set! x x^)
                               (init super x* y* hue*)))
                       (e (let ((y 5))
                            (method (q r . args)
                              (+ x y q r (car args)))))))))
               (lambda (s)
                 (create-class <<escp>> s))))))

(define <escp> ((<escp>-maker 1) <scp>))

(define test-<escp>
  (lambda ()
    (let ((escp (_n <escp>)))
      ((_mp escp 1) escp 10 20 7)
      ((_mv escp 'e) escp 1 1 1))))

(write (test-<escp>))
(newline)

;;; Section 9
(define-syntax new
  (syntax-rules ()
    ((_ c arg ...)
     (let ((o (_n c)))
       ((_mp o 1) o arg ...)
       o))))

(define-syntax mbv
  (syntax-rules ()
    ((_ m oc arg ...)
     (let ((oc^ oc))
       ((_mv oc^ 'm) oc^ arg ...)))))

(define-syntax invoke
  (syntax-rules ()
    ((_ shadow m oc arg ...)
     (let ((oc^ oc))
       ((shadow m oc^) oc^ arg ...)))))

(define isa?
  (lambda (this c)
    ((_mp this 0) this c)))

(define test-<scp>
  (lambda ()
    (let ((p (new <p> 1 2))
          (scp (new <scp> 18 19 9)))
      (invoke <<scp>> diag&set scp 10)
      (list
        (list
          (invoke <<scp>> get-loc scp)
          (invoke <<cp>> get-hue scp))
        (isa? p <scp>)
        (isa? scp <p>)))))

(define <scp> (create-class <<scp>> <cp>))
(pretty-print (test <scp>))

;;;; Code of Section 2

(define-syntax <<o>>
  (build-<<o>>
    ((isa? (method (c) (_mteq? this c)))
     (init (method args (void))))))

(define-syntax <<p>>
  (extend-shadow <<o>> (x y)
    ((init
       (method (x^ y^)
         (set! x x^)
         (set! y y^)
         (init super)))
     (move
       (method (dx dy)
         (set! x (+ x dx))
         (set! y (+ y dy))))
     (get-loc
       (method ()
         (list x y)))
     (diag
       (method (a)
         (move this a a))))))

(define-syntax <<cp>>
  (extend-shadow <<p>> (hue)
    ((init
       (method (x^ y^ hue^)
         (set! hue hue^)
         (init super x^ y^)))
     (get-hue
       (method () hue))
     (diag&set
       (method (a)
         (diag this a)
         (set! hue a))))))

(define-syntax <<scp>>
  (extend-shadow <<cp>> (y)
    ((init
       (method (x^ y^ hue^)
         (set! y ": Stuck: ")
         (init super x^ y^ hue^)))
     (move
       (method (x^ y^)
         (show-y this)))
     (diag
       (method (a)
         (write hue)
         (diag super a)))
     (show-y
       (method ()
         (display y))))))

(define <o> (create-class <<o>> #f))
(define <p> (create-class <<p>> <o>))
(define <cp> (create-class <<cp>> <p>))
(define <scp> (create-class <<scp>> <cp>))

(pretty-print (expand '(create-class <<o>> #f)))
(pretty-print (expand '(create-class <<p>> <o>)))
(pretty-print (expand '(create-class <<cp>> <p>)))
(pretty-print (expand '(create-class <<scp>> <cp>)))

(define <scp>
  (let ((super <cp>))
    (let ((h0 (_mp super 0))
          (h1 (_mp super 1))
          (h2 (_mp super 2))
          (h3 (_mp super 3))
          (h4 (_mp super 4))
          (h5 (_mp super 5))
          (h6 (_mp super 6)))
      (let ((g0
              (lambda (this c)
                ((lambda (t) (if t t ((_mp super 0) super c)))
                 (_mteq? this c))))
            (g1
              (lambda (this x^ y^ hue^)
                (_fp! this 3 ": Stuck: ")
                (h1 this x^ y^ hue^)))
            (g2
              (lambda (this x^ y^)
                ((lambda (oc^) ((_mp oc^ 7) oc^)) this)))
            (g3 (lambda (this a) (write (_fp this 2)) (h4 this a)))
            (g4 (lambda (this) (display (_fp this 3)))))
        (let ((isa? h0)
              (init h1)
              (move h2)
              (get-loc h3)
              (diag h4)
              (get-hue h5)
              (diag&set h6))
          (let ((isa? g0)
                (init g1)
                (move g2)
                (diag g3)
                (show-y g4))
            (list
              '(x y hue y)
              '(isa? init move get-loc diag get-hue diag&set show-y)
              (vector isa? init move get-loc diag get-hue diag&set show-y))))))))

(define test
  (lambda (<cp>^)
    (let ((p (new <p> 1 2))
          (cp (new <cp>^ 18 19 9)))
      (mbv diag&set cp 10)
      (list
        (list
          (invoke <<cp>> get-loc cp)
          (invoke <<cp>> get-hue cp))
        (isa? p <cp>^)
        (isa? cp <p>)))))

(begin (write (test <scp>)) (write (test <cp>)))
(newline)

;;; Oleg's test of scope; answer should be (5 7 5).
(define <test>-maker
  (lambda (x)
    (let-syntax
        ((<<obj>>
           (extend-shadow <<o>> (x)
             ((init (method (x^)
              (set! x x^)))
          (get-x (method () x))))))
      (let
    ((<obj1> (create-class <<obj>> <o>))
     (<obj2> (create-class <<obj>> <o>)))
    (let*
      ((obj1 (_n <obj1>))
       (obj2 (_n <obj2>))
       (_  ((_mv obj1 'init) obj1 5))
       (v1 ((_mv obj1 'get-x) obj1))
       (_  ((_mv obj2 'init) obj2 7))
       (v2 ((_mv obj2 'get-x) obj2))
       (v3 ((_mv obj1 'get-x) obj1))
      )
      (list v1 v2 v3))))))

(pretty-print (<test>-maker 0))

(pretty-print
  (expand
    '(lambda (x)
       (let-syntax
           ((<<obj>>
              (extend-shadow <<o>> (x)
                ((init (method (x^)
                         (set! x x^)))
                 (get-x (method () x))))))
         (let
             ((<obj1> (create-class <<obj>> <o>))
              (<obj2> (create-class <<obj>> <o>)))
           (let*
               ((obj1 (_n <obj1>))
                (obj2 (_n <obj2>))
                (_  ((_mv obj1 'init) obj1 5))
                (v1 ((_mv obj1 'get-x) obj1))
                (_  ((_mv obj2 'init) obj2 7))
                (v2 ((_mv obj2 'get-x) obj2))
                (v3 ((_mv obj1 'get-x) obj1)))
             (list v1 v2 v3)))))))

(printf "------ START ERIK HILSDALE's CODE -----~n")

;; rewrite
(define-syntax build-shadow
  (lambda (x)
    (syntax-case x ()
      ((_ ctx sup-shadow sup-f sup-m (f-var ...) ((m-var g e) ...))
       (let ((sup-f (syntax-object->datum (syntax sup-f)))
             (sup-m (syntax-object->datum (syntax sup-m)))
             (f-vars (syntax-object->datum (syntax (f-var ...))))
             (m-vars (syntax-object->datum (syntax (m-var ...)))))
         (let ((f (append-env sup-f f-vars))
               (m (append-env sup-m (fresh-m-vars m-vars sup-m))))
           (with-syntax
             ((((s k) ...) (datum->syntax-object (syntax ctx) (enumerate-env sup-m)))
              (((m i) ...) (datum->syntax-object (syntax ctx) (enumerate-env m)))
              (((f j) ...) (datum->syntax-object (syntax ctx) (trim-env (enumerate-env f))))
              ((all-f ...) (datum->syntax-object (syntax ctx) f))
              ((my-cookie) (generate-temporaries '(blah))))
             (syntax (lambda (xx)
                 (syntax-case xx (is-me? cookie-equals?)
                   ((__ cookie-equals? cookie sk fk)            ;; NEW CLAUSES
                    (free-identifier=? (syntax cookie) (syntax my-cookie))
                    (syntax sk))
                   ((__ cookie-equals? cookie sk fk)
                    (syntax fk))
                   ((__ is-me? x)
                    (syntax (x cookie-equals? my-cookie #t (sup-shadow is-me? x)))) ;; END NEW CLAUSES
                   ((__)
                    (syntax (extender ctx ((s k) ...) (all-f ...) ((f j) ...) ((m i) ...) ((m-var g e) ...))))
                   ((__ an-m-var oc) (syntax (_mp oc (assv-macro an-m-var ((m i) ...)))))
                   ((__ ctx sup-shadow^ (f-var^ (... ...)) ((m-var^ e^) (... ...)))
                    (with-syntax (((g^ (... ...)) (generate-temporaries (syntax (m-var^ (... ...))))))
                      (syntax (build-shadow ctx sup-shadow^ (all-f ...) (m ...)
                          (f-var^ (... ...))
                          ((m-var^ g^ e^) (... ...))))))))))))))))

(define-syntax <<no-shadow>>
  (syntax-rules (is-me?)
    ((_ is-me? x) #f)))

;; rewrite
(define-syntax build-<<o>>
  (lambda (x)
    (syntax-case x (is-me? cookie-equals?)
      ((_ ((m e) ...))
       (with-syntax
           (((g ...) (generate-temporaries (syntax (m ...)))))
         (syntax (build-shadow _ <<no-shadow>> () () ()
             ((m g e) ...))))))))

;; add
(define-syntax <<o>>
  (build-<<o>>
    ((isa? (lambda (this c) (_mteq? this c)))
     (init (lambda (this . args) (void))))))

(define <o> (create-class <<o>> #f))

;; add
(define-syntax shadow-isa?
  (syntax-rules ()
    ((_ x y)
     (x is-me? y))))

;; rewrite
(define-syntax extend-shadow
  (lambda (x)
    (syntax-case x ()
      ((_ sup-shadow (f-var ...) ((m-var e) ...))
       (with-implicit (_ super isa?)
         (syntax (sup-shadow _ sup-shadow (f-var ...) ;; PASSING sup-shadow DOWN
             ((isa? (lambda (this c)
                      (or (_mteq? this c)
                        ((_mp super 0) super c))))
              (m-var e) ...))))))))

;; TESTS

;; TESTS

(define-syntax <<a>> (extend-shadow <<o>> () ()))
(define-syntax <<b>> (extend-shadow <<a>> () ()))
(define-syntax <<c>> (extend-shadow <<o>> () ()))

(define-syntax ensure
  (syntax-rules ()
    ((_ test result)
     (let ((r test))
       (if (equal? r result)
           (printf "(PASS) ~s => ~s~n" 'test r)
           (printf "(FAIL) ~s => ~s (wanted ~s)~n" 'test r 'result))))))

;; everything isa itself
(ensure (expand '(shadow-isa? <<o>> <<o>>)) #t)
(ensure (expand '(shadow-isa? <<a>> <<a>>)) #t)
(ensure (expand '(shadow-isa? <<b>> <<b>>)) #t)
(ensure (expand '(shadow-isa? <<c>> <<c>>)) #t)

;; everything isa <<o>>
(ensure (expand '(shadow-isa? <<a>> <<o>>)) #t)
(ensure (expand '(shadow-isa? <<b>> <<o>>)) #t)
(ensure (expand '(shadow-isa? <<c>> <<o>>)) #t)

;; <<o>> isa nothing but itself
(ensure (expand '(shadow-isa? <<o>> <<a>>)) #f)
(ensure (expand '(shadow-isa? <<o>> <<b>>)) #f)
(ensure (expand '(shadow-isa? <<o>> <<c>>)) #f)

;; <<c>> is unrelated to <<a>> and <<b>>
(ensure (expand '(shadow-isa? <<c>> <<a>>)) #f)
(ensure (expand '(shadow-isa? <<c>> <<b>>)) #f)
(ensure (expand '(shadow-isa? <<a>> <<c>>)) #f)
(ensure (expand '(shadow-isa? <<b>> <<c>>)) #f)

;; <<b>> isa <<a>> but not vice versa
(ensure (expand '(shadow-isa? <<b>> <<a>>)) #t)
(ensure (expand '(shadow-isa? <<a>> <<b>>)) #f)

;;; Code to generate code in JS.java.
;;; For example: (show (gen 20 template2))
(define (jsArg n) {a[n]})
(define (jsParameter n) {Object [(jsArg n)]})
(define (List x) {([x])})
(define (many what n sofar)
  (if (= n 0) sofar
      (many what (- n 1) (cons (what n) sofar))))
(define (jsParameters n) (sep ", " (many jsParameter n '())))
(define (sep by xs)
  (if (null? xs) ""
      (if (null? (cdr xs)) (car xs)
	  {[(car xs)][by][(sep by (cdr xs))]})))
(define (jsArgs n) (sep ", " (many jsArg n '())))

(define (template1 n)
{  public Object call(String p,[(jsParameters n)]) \{
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list([(jsArgs n)])); \}
    finally { exit(); \} \}
})

(define (template2 n)
{  public Object call(SchemeProcedure p, [(jsParameters n)]) \{
    enter(); try {
      return p.apply(list([(jsArgs n)])); \}
    finally { exit(); \} \}
})

(define (pairs args)
  (if (null? args) "Pair.EMPTY"
      {new Pair([(car args)], [(pairs (cdr args))])}))
      

(define (template3 n)
{  public static SchemePair list(Object a0, [(jsParameters n)]) \{
    return new Pair(a0, list([(jsArgs n)])); \}
})

(define (gen n what)
  (if (= n 0) (what n)
      {[(gen (- n 1) what)][(what n)]}))

(define (show x)
  (display x)
  (newline))

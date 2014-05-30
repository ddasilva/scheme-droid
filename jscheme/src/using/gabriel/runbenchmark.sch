;;;
;;; You need to write a procedure named "run-benchmark" that takes
;;; two arguments.  The first is a string identifying the particular
;;; benchmark being run.  The second is a thunk, a procedure of no
;;; arguments, that will actually run the benchmark.
;;;

(define (run-benchmark benchmark-name benchmark-thunk)
  ;;; your code goes here
  (display {[benchmark-name]::})
  (display (.toString (tryCatch (second (time-call benchmark-thunk 1))(lambda(e) {FAILURE: [e]}))))
  (newline)
  #t
)

(define benchmarks
  '(
          ;boyer.sch   ;; doesn't work....
          ;browse.sch  ;; requires () = #f which is not true for JScheme
       cpstack.sch
       ctak.sch
       dderiv.sch
       deriv.sch
       destruct.sch
       div.sch
       fft.sch
       fprint.sch
       fread.sch
       puzzle.sch
       tak.sch
       takl.sch
       takr.sch
          ;tprint.sch  ;; throws an exception...
          ;traverse.sch ;; depends on ()=#f
      )
)


(define (runall)
  (for-each 
     (lambda(x) (load {using/gabriel/[x]}))
      benchmarks
  ))

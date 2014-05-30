(load "elf/eopl2/jscheme/genwrite.scm")
(load "elf/eopl2/jscheme/psyntax-init.scm")

;; At this point eval and load have been redefined to use
;; define-syntax.  Code that needs define-syntax should be loaded by
;; this file.

(define (remv x ls)
  (cond ((null? ls) ls)
	((eqv? (car ls) x) (remv x (cdr ls)))
	(else (cons (car ls) (remv x (cdr ls))))))

(define (printf x)
  (display x)
  (newline))

(define (print-gensym x) x)		; No op.

(define expand sc-expand)

(load "using/friedman6.scm") 
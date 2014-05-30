;; getformvalues.scm
;; read multiple values from a form, good for checkboxes and lists

(define (get-form-values request x)
  (tryCatch
    (array->list 
      (.getParameterValues request x))
    (lambda(e) ())))

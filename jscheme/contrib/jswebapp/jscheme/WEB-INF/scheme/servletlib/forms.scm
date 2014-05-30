;; forms.scm
;; simple way to make a form

(define make-form
  (lambda (url . args)
    {
     <form method="get" action="[url]">
      [(map
        (lambda(x)
          {[x]: <input type=text name="[x]"><br>\n})
       args)]
     <input type="submit">
    </form>
   }
 )
)

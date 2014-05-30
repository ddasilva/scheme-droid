;; redirect.scm

(begin
  ; use -- (redirect response "lib/init.servlet")
  (define (redirect response localpath)
    (.sendRedirect response  {webapps[context]/[localpath]}))

  (basic-webpage {redirect.scm} {(redirect ...)  has been defined})
)

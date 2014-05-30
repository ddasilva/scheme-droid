{
  classbrowser.scm --
    to run as an applet, append "app" to the end of the scheme file,
    that is, view the file classbrowser.scmapp
  Or you can run it from java directly as
  % java jscheme.REPL classbrowser.scm '(install #null)'
}
(use-module "jlib/Swing.scm")
(use-module "elf/basic.scm")

(define (install the-applet) 
   ; note we don't actually use the-applet, but the .scmapp servlet expects it
  (define w (window "class browser"))
  (define (get-superclasses X)
    (if (equal? X #null) ()
        (cons X (get-superclasses (.getSuperclass X)))))
  (define (ol-list L) {<ol>[(map* (lambda(x) {<li>[x]</li>\n}) L)] </ol>})

  (define doit (action (lambda(e) 
    (define the-class (eval(readexpr tf)))
    (writestring ta "")
    (appendstring ta (describe-class the-class))
   )))

  (define (describe-class the-class)
      {<html>
         <div style="background:rgb(200,200,255); padding:0.5in">
<h1>Description of [the-class]</h1>
         <h2>Superclassses</h2>[(ol-list (get-superclasses the-class))]<br>
         <h2>Interfaces</h2>[(ol-list (.getInterfaces the-class))]<br>
         <h2>Constructors</h2> [(ol-list (.getConstructors the-class))]<br>
         <h2>Methods:</h2>[(ol-list (.getMethods the-class))]<br>
         <h2>Fields</h2>[(ol-list (.getFields the-class))]<br>
         </div>
         </html>
      }
   )

  (define ta (label {<html><div style="height:3in">...</div></html>}))
  (define tf (textfield 40 "javax.swing.JButton.class"))
  (define w
   (window "class browser"
    (border
      (north (label "Class Browsers"))
      (center (scrollpane ta))
      (south (row (button "describe class" doit) tf)))))
  (.pack w)
  (.show w)

)


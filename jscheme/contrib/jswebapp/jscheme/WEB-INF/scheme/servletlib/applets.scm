(define (applet request W H File)
 {
  <applet
     code="jsint.SchemeApplet"
     codebase="."
     archive="[(.getContextPath request)]/lib/applet.jar"
     width=[W]
     height=[H]
     >
   <param name="prog" value="[File]">
   <param name=init value="install">
  </applet>
 }
)

(define (multi-jar-applet request W H jars File)
  (define (fullpath x) 
    {[(.getContextPath request)]/lib/[x].jar})

  ; create a list of jar urls, separated by commas, ending with the applet.jar file
  ; all jars are assumed to be in the toplevel lib folder 
  (define (url-list L)
    {[(map (lambda(x) {[(fullpath x)],}) L)][(fullpath "applet.jar")]})

 {
  <applet
     code="jsint.SchemeApplet"
     codebase="."
     archive="[(url-list jars)]"
     width=[W]
     height=[H]
     >
   <param name="prog" value="[File]">
   <param name=init value="install">
  </applet>
 }
)

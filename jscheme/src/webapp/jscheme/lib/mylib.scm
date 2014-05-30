;; mylib.scm
   (define (captioned-image C I)
    {<table border=5>
        <tr><td>
          <img src="[I]" alt="[C]">
       </td></tr>
        <tr><td>[C]
      </td></tr> </table>})

   (define (generic-page Title CSS Body)
     {<html>
         <head><title> [Title]</title>
          <style type="text/css" media="screen">
            <!-- [CSS] --></style></head>
       <body> [Body]</body>
     </html>})

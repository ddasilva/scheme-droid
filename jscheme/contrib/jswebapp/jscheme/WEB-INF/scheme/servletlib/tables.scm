 (define (ol L) {<ol>[(lis L)]</ol>})
 (define (ul L) {<ul>[(lis L)]</ul>})
 (define (lis L) 
   (define (li x) {<li>[x]</li>})
    (map li L))

 (define (table Rs)
    { <table border=5 cellpadding=5 cellspacing=5>
       [(trs Rs)]
     </table>\n\n})
 (define (trs Rs) 
   (define (td X)  {    <td]>[X]</td>\n})
   (define (tds Ts) (map td Ts))
   (define (tr Ts) {  <tr> [(tds Ts)] </tr>\n})
   (map tr Rs))



{
http://sax.sourceforge.net

This class is a SAXHandler that converts XML input into a symbolic
expression.  This uses the JAXP approach built in to JDK 1.4.  

Relatively naive structure sharing is used to save storage space.
While DOM can be 10 times the size of the input XML, with
structure sharing, the resulting s-expression can be about the
same size (in memory) as the XML text.


Sample usage:
(.parse (elf.SaxExp.)
  (java.net.URL. "http://www.w3.org/1999/02/22-rdf-syntax-ns"))
}
(define-class
  (package elf)

  (import java.io.Reader)
  (import java.util.Hashtable)
  (import org.xml.sax.Attributes)
  (import org.xml.sax.InputSource)
  (import org.xml.sax.Locator)
  (import org.xml.sax.SAXParseException)
  (import org.xml.sax.helpers.DefaultHandler)
  (import jsint.Pair)

  (public class SaxExp extends DefaultHandler)

  (public SaxExp () #f)

  (public SaxExp (boolean keepMixedContent)
	  (.keepMixedContent$ this (Boolean. keepMixedContent)))
  
  ;; Keep characters between an end tag and the beginning of the next
  ;; start tag.  Often these are just whitespace, but sometimes they
  ;; have meaning.
  (public boolean keepMixedContent = #f)

  (static
   (load "elf/classpath.scm")
   (import "org.xml.sax.helpers.XMLReaderFactory")
   (import "org.xml.sax.InputSource")
   )

  ;; Parse XML in Reader to SEXP
  (public Object parse (Reader reader)
    (let ((xr (.getXMLReader
	       (.newSAXParser
		(javax.xml.parsers.SAXParserFactory.newInstance)))))
      (.setContentHandler xr this)
      (.setErrorHandler xr this)
      (.parse xr (InputSource. reader)))
    (let ((result (.pop this)))
      (.clear (.table$ this))
      (.stack$ this '())
      result))

  (public Object parse (java.net.URL url)
    (.parse this (java.io.BufferedReader. (java.io.InputStreamReader.
					   (.openStream url)))))
  ;; Intern to share structure.
  (public Hashtable table = (java.util.Hashtable. 500))
  (public Object intern (Object x)
    (let ((it (.get (.table$ this) x)))
      (if (isNull it)
	  (begin (.put (.table$ this) x x)
		 x)
	  it)))

  ;; Component stack.
  (public Pair stack = '())
  (public Object isEmpty ()
    (null? (.stack$ this)))
  (public Object top ()
    (if (null? (.stack$ this))
	(error this " stack is null!"))
    (car (.stack$ this)))

  (public void push(Object x)
    (.stack$ this (cons x (.stack$ this)))) 
  (public Object pop ()
    (if (null? (.stack$ this))
	(error this " stack is null!"))
    (let ((pairs (.stack$ this)))
      (.stack$ this (cdr pairs))
      (car pairs)))

  (public jsint.Symbol lastState)
  ;; Override the DefaultHandler methods:
  (public void characters(char[] ch int start int length)
    (if (.keepMixedContent$ this)
	(.addCharacters this (String. ch start length))
	;; Ignore any characters after an endElement
	;; Ignore "\n" anywhere.
	(if (not (eq? (.lastState$ this) 'endElement))
	    (let ((s (String. ch start length)))
	      (if (not (.equals s "\n"))
		  (.addCharacters this s))
	      (.lastState$ this 'characters)))))

  ;; Add characters, either by appending them to the string at the top
  ;; of the stack, or by pushing a new string.
  (public void addCharacters (String s)
    (if (and (not (.isEmpty this)) (string? (.top this)))
	(.push this (string-append (.pop this) s))
	(.push this s)))

  (public void endDocument() #f)
  
  (public void endElement(String uri String localName String qName)
    ;; (print `(endElement ,uri ,localName ,qName))
    (let ((name (string->symbol qName)))
      (let loop ((it (.top this))
		 (sofar '()))
	(if (or (eq? it name) (and (pair? it) (eq? (car it) name)))
	    (.push this (cons (.pop this) sofar))
	    (let ((sofar (cons (.pop this) sofar)))
	      (loop (.top this) sofar)))))
    (.lastState$ this 'endElement))

  (public void endPrefixMapping(String prefix)
    (print `(endPrefixMapping ,prefix)))

  (public void error(SAXParseException e)
    (throw e))

  (public void fatalError(SAXParseException e)
    (throw e))

  (public void ignorableWhitespace(char[] ch int start int length)
    (print `(ignorable ,(String. ch start end)))
    #f)

  (public void notationDecl(String name String publicId String systemId)
    (print `(notationDecl ,name ,publicId ,systemId)))
  
  (public void processingInstruction(String target String data)
    (print `(processingInstruction ,target ,data)))

  (public InputSource resolveEntity(String publicId String systemId)
    ;; Default behavior is to return null.
    #null)

  (public void setDocumentLocator(Locator locator) #f)
  
  (public void skippedEntity(String name)
    (print `(skippedEntry ,name)))
	  
  (public void startDocument() #f)

  (public void startElement
	  (String uri String localName String qName Attributes attributes)
    (define (mapAttributes as f)
      (define (attributes0 i)
	(if (< i (.getLength as))
	    (cons (f as i) (attributes0 (+ i 1)))
	    '()))
      (attributes0 0))
    (define (makeAttribute as i)
      (let ((type (.getType as i))
	    (value (.getValue as i)))
	(if (not (equal? type "CDATA"))
	    (.intern this `(,(string->symbol (.getQName as i))
			    ,(string->symbol value)))
	    `(,(string->symbol (.getQName as i))
	      ,value))))
    ;; (print `(startElement ,uri ,localName ,qName ,attributes))
    (let* ((as (mapAttributes attributes makeAttribute))
	   (element (if (null? as) (string->symbol qName)
			(cons (string->symbol qName) as))))
      (.push this element))
    (.lastState$ this 'startElement))

  (public void startPrefixMapping(String prefix String uri)
    (print `(statPrefixMapping ,prefix ,uri)))

  (public void unparsedEntityDecl
	  (String name String publicId String systemId String notationName)
    (print `(unparseEntityDecl ,name ,publicId ,systemId
			       ,notationName)))

  (public void warning(SAXParseException e)
    (jsint.E.warn e))
  )

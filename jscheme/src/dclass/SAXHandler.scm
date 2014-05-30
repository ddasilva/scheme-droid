;;; http://sax.sourceforge.net

;;; This is a simple SAX handler that prints each event.
(define-class
  (package elf)

  (import java.io.Reader)
  (import org.xml.sax.Attributes)
  (import org.xml.sax.InputSource)
  (import org.xml.sax.Locator)
  (import org.xml.sax.SAXParseException)
  (import org.xml.sax.helpers.DefaultHandler)

  (static
   (load "elf/classpath.scm")
   (import "org.xml.sax.helpers.XMLReaderFactory")
   (import "org.xml.sax.InputSource")

   ;; Tell SAX what parser to use.  This one is part of JDK 1.4.
   (System.setProperty "org.xml.sax.driver"
		       "org.apache.crimson.parser.XMLReaderImpl")
   )

  ;; Parse XML in Reader to SEXP
  (public Object parse (Reader reader)
	  (let ((xr (XMLReaderFactory.createXMLReader)))
	    (.setContentHandler xr this)
	    (.setErrorHandler xr this)
	    (.parse xr (InputSource. reader))))

  (public class SAXHandler extends DefaultHandler)

  (public void characters(char[] ch int start int length)
	  (print `(characters ,(String. ch start length))))

  (public void endDocument()
	  (print `(endDocument)))
  
  (public void endElement(String uri String localName String qName)
	  (print `(endElement ,uri ,localName ,qName)))

  (public void endPrefixMapping(String prefix)
	  (print `(endPrefixMapping ,prefix)))

  (public void error(SAXParseException e)
	  (throw e))

  (public void fatalError(SAXParseException e)
	  (throw e))

  (public void ignorableWhitespace(char[] ch int start int length) #f)

  (public void notationDecl(String name String publicId String systemId)
	  (print `(notationDecl ,name ,publicId ,systemId)))
  
  (public void processingInstruction(String target String data)
	  (print `(processingInstruction ,target ,data)))

  (public InputSource resolveEntity(String publicId String systemId)
	       ;; Default behavior is to return null.
	       #null)

  (public void setDocumentLocator(Locator locator)
	  (print `(setDocumentLocator ,locator)))
  
  (public void skippedEntity(String name)
	  (print `(skippedEntry ,name)))
	  
  (public void startDocument()
	  (print `(startDocument)))

  (public void startElement
	  (String uri String localName String qName Attributes attributes)
	  (print `(startElement
		   ;; ,uri
		   ;; ,localName
		   ,qName
		   ,(map-attributes attributes attribute-parts))))

  (public void startPrefixMapping(String prefix String uri)
	  (print `(statPrefixMapping ,prefix ,uri)))

  (public void unparsedEntityDecl
	  (String name String publicId String systemId String notationName)
	  (print `(unparseEntityDecl ,name ,publicId ,systemId
				     ,notationName)))

  (public void warning(SAXParseException e)
	  (jsint.E.warn e))
  )
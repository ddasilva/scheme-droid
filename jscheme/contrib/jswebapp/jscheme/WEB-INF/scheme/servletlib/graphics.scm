;; graphics .scm
;; Lets you create a webpage that returns a dynamically drawn jpg....
;; (drawfn g) is a procedure which will draw something on the graphics component g

(define (send-jpg response w h drawfn)
  (define awtImage (java.awt.image.BufferedImage. w h java.awt.image.BufferedImage.TYPE_INT_RGB$))
  (define g (.getGraphics awtImage))			
  (drawfn g)
  (.setContentType response "image/jpeg")
  (let* 
     ((out (java.io.ByteArrayOutputStream.))
      (j   (sun.awt.image.codec.JPEGImageEncoderImpl. out))
      (jpegParams (.getDefaultJPEGEncodeParam j awtImage))
     )
    (.setQuality jpegParams 1.0f #f)
    (.setJPEGEncodeParam j jpegParams)
    (.encode j awtImage)
    (.close out)
    (.writeTo out (.getOutputStream response))
  #null))



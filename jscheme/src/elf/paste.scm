;;; WELCOME to the Java Developer Connection(sm) Tech Tips, Vol. 2 No. 9.
;;; This issue covers:
;;;                      * Cut, copy, and paste
;;;                      * Package version identification

;;; Imports for clipboard.
(import "java.awt.Toolkit")
(import "java.awt.datatransfer.Clipboard")
(import "java.awt.datatransfer.DataFlavor")
(import "java.awt.datatransfer.StringSelection")

;;; Imports for paste-demo.
(import "java.awt.event.WindowEvent")
(import "javax.swing.JButton")
(import "javax.swing.JFrame")
(import "javax.swing.JPanel")
(import "javax.swing.JTextArea")

(define clipboard (.getSystemClipboard (Toolkit.getDefaultToolkit)))

(define (toClipboard string)
  (.setContents clipboard (StringSelection. string) #null))

(define (fromClipboard)
  (.getTransferData (.getContents clipboard #null) DataFlavor.stringFlavor$))

(define (button panel name action)
  (let ((b (JButton. name)))
    (.add panel b)
    (.addActionListener b (Listener. action))
    b))

(define (paste-demo)
  (let* ((frame (JFrame. "Clipboard demo"))
	 (panel (.getContentPane frame))
	 (textarea (JTextArea. 10 40))
	 (buttonPanel (JPanel.))
	 (copyButton (button buttonPanel "Copy" 
			     (lambda (e) 
			       (toClipboard (.getSelectedText textarea)))))
	 (pasteButton (button buttonPanel "Paste" 
			      (lambda (e)
				(.setText textarea (fromClipboard))))))
    (.addWindowListener
     frame
     (Listener.
      (lambda (e) (if (= (.getID e) WindowEvent.WINDOW_CLOSING$)
		      (.dispose (.getWindow e))))))
    (.add panel "Center" textarea)
    (.add panel "South" buttonPanel)
    (.pack frame)
    (.setVisible frame #t)
    frame))
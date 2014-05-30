package jsint;

/** This class allows a Procedure to act as a listener to many events.
    
	    For example, to add an action listener to a button, b:
    
	    <pre>
	    (import "java.awt.*")
	    (import "javax.swing.*")
	    (let ((f (JFrame. "Example"))
		  (b (JButton. "Press Me")))
	      (.addActionListener 
	       b
	       (Listener. (lambda (e) (.println (System.out$) "Yow!"))))
	      (.add (.getContentPane f) b (BorderLayout.CENTER$))
	      (.pack f)
	      (.show f))
	    </pre>
	    NOTE: Listener.java IS GENERATED FROM listener.scm. EDIT AT YOUR OWN
	    RISK.
	    **/ 
	    
public class Listener extends Listener11swing implements java.awt.event.InputMethodListener, java.util.EventListener, java.awt.event.AWTEventListener{
  public Listener(Procedure handler) {
    super(handler);
  }
 
  public void inputMethodTextChanged(java.awt.event.InputMethodEvent e) {
        handler.apply(U.list(e));
      }
    
  public void caretPositionChanged(java.awt.event.InputMethodEvent e) {
        handler.apply(U.list(e));
      }
    
  public void eventDispatched(java.awt.AWTEvent e) {
        handler.apply(U.list(e));
      }
    
}

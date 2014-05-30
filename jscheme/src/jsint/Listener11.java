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
	       (Listener11. (lambda (e) (.println (System.out$) "Yow!"))))
	      (.add (.getContentPane f) b (BorderLayout.CENTER$))
	      (.pack f)
	      (.show f))
	    </pre>
	    NOTE: Listener11.java IS GENERATED FROM listener.scm. EDIT AT YOUR OWN
	    RISK.
	    **/ 
	    
public class Listener11 extends JavaListener implements java.awt.event.WindowListener, java.awt.event.TextListener, java.awt.event.MouseMotionListener, java.awt.event.MouseListener, java.awt.event.KeyListener, java.awt.event.ItemListener, java.awt.event.FocusListener, java.awt.event.ContainerListener, java.awt.event.ComponentListener, java.awt.event.AdjustmentListener, java.util.EventListener, java.awt.event.ActionListener{
  public Listener11(Procedure handler) {
    super(handler);
  }
 
  public void windowOpened(java.awt.event.WindowEvent e) {
        handler.apply(U.list(e));
      }
    
  public void windowClosing(java.awt.event.WindowEvent e) {
        handler.apply(U.list(e));
      }
    
  public void windowClosed(java.awt.event.WindowEvent e) {
        handler.apply(U.list(e));
      }
    
  public void windowIconified(java.awt.event.WindowEvent e) {
        handler.apply(U.list(e));
      }
    
  public void windowDeiconified(java.awt.event.WindowEvent e) {
        handler.apply(U.list(e));
      }
    
  public void windowActivated(java.awt.event.WindowEvent e) {
        handler.apply(U.list(e));
      }
    
  public void windowDeactivated(java.awt.event.WindowEvent e) {
        handler.apply(U.list(e));
      }
    
  public void textValueChanged(java.awt.event.TextEvent e) {
        handler.apply(U.list(e));
      }
    
  public void mouseMoved(java.awt.event.MouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void mouseDragged(java.awt.event.MouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void mousePressed(java.awt.event.MouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void mouseReleased(java.awt.event.MouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void mouseClicked(java.awt.event.MouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void mouseExited(java.awt.event.MouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void mouseEntered(java.awt.event.MouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void keyTyped(java.awt.event.KeyEvent e) {
        handler.apply(U.list(e));
      }
    
  public void keyPressed(java.awt.event.KeyEvent e) {
        handler.apply(U.list(e));
      }
    
  public void keyReleased(java.awt.event.KeyEvent e) {
        handler.apply(U.list(e));
      }
    
  public void itemStateChanged(java.awt.event.ItemEvent e) {
        handler.apply(U.list(e));
      }
    
  public void focusGained(java.awt.event.FocusEvent e) {
        handler.apply(U.list(e));
      }
    
  public void focusLost(java.awt.event.FocusEvent e) {
        handler.apply(U.list(e));
      }
    
  public void componentAdded(java.awt.event.ContainerEvent e) {
        handler.apply(U.list(e));
      }
    
  public void componentRemoved(java.awt.event.ContainerEvent e) {
        handler.apply(U.list(e));
      }
    
  public void componentResized(java.awt.event.ComponentEvent e) {
        handler.apply(U.list(e));
      }
    
  public void componentMoved(java.awt.event.ComponentEvent e) {
        handler.apply(U.list(e));
      }
    
  public void componentShown(java.awt.event.ComponentEvent e) {
        handler.apply(U.list(e));
      }
    
  public void componentHidden(java.awt.event.ComponentEvent e) {
        handler.apply(U.list(e));
      }
    
  public void adjustmentValueChanged(java.awt.event.AdjustmentEvent e) {
        handler.apply(U.list(e));
      }
    
  public void actionPerformed(java.awt.event.ActionEvent e) {
        handler.apply(U.list(e));
      }
    
}

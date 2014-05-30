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
	       (Listener11swing. (lambda (e) (.println (System.out$) "Yow!"))))
	      (.add (.getContentPane f) b (BorderLayout.CENTER$))
	      (.pack f)
	      (.show f))
	    </pre>
	    NOTE: Listener11swing.java IS GENERATED FROM listener.scm. EDIT AT YOUR OWN
	    RISK.
	    **/ 
	    
public class Listener11swing extends Listener11 implements javax.swing.event.UndoableEditListener, javax.swing.event.TreeWillExpandListener, javax.swing.event.TreeSelectionListener, javax.swing.event.TreeModelListener, javax.swing.event.TreeExpansionListener, javax.swing.event.TableModelListener, javax.swing.event.TableColumnModelListener, javax.swing.event.PopupMenuListener, java.awt.event.MouseMotionListener, java.awt.event.MouseListener, javax.swing.event.MouseInputListener, javax.swing.event.MenuListener, javax.swing.event.MenuKeyListener, javax.swing.event.MenuDragMouseListener, javax.swing.event.ListSelectionListener, javax.swing.event.ListDataListener, javax.swing.event.InternalFrameListener, javax.swing.event.HyperlinkListener, javax.swing.event.DocumentListener, javax.swing.event.ChangeListener, javax.swing.event.CellEditorListener, javax.swing.event.CaretListener, java.util.EventListener, javax.swing.event.AncestorListener{
  public Listener11swing(Procedure handler) {
    super(handler);
  }
 
  public void undoableEditHappened(javax.swing.event.UndoableEditEvent e) {
        handler.apply(U.list(e));
      }
    
  public void treeWillExpand(javax.swing.event.TreeExpansionEvent e) {
        handler.apply(U.list(e));
      }
    
  public void treeWillCollapse(javax.swing.event.TreeExpansionEvent e) {
        handler.apply(U.list(e));
      }
    
  public void valueChanged(javax.swing.event.TreeSelectionEvent e) {
        handler.apply(U.list(e));
      }
    
  public void treeNodesChanged(javax.swing.event.TreeModelEvent e) {
        handler.apply(U.list(e));
      }
    
  public void treeNodesInserted(javax.swing.event.TreeModelEvent e) {
        handler.apply(U.list(e));
      }
    
  public void treeNodesRemoved(javax.swing.event.TreeModelEvent e) {
        handler.apply(U.list(e));
      }
    
  public void treeStructureChanged(javax.swing.event.TreeModelEvent e) {
        handler.apply(U.list(e));
      }
    
  public void treeExpanded(javax.swing.event.TreeExpansionEvent e) {
        handler.apply(U.list(e));
      }
    
  public void treeCollapsed(javax.swing.event.TreeExpansionEvent e) {
        handler.apply(U.list(e));
      }
    
  public void tableChanged(javax.swing.event.TableModelEvent e) {
        handler.apply(U.list(e));
      }
    
  public void columnAdded(javax.swing.event.TableColumnModelEvent e) {
        handler.apply(U.list(e));
      }
    
  public void columnRemoved(javax.swing.event.TableColumnModelEvent e) {
        handler.apply(U.list(e));
      }
    
  public void columnMoved(javax.swing.event.TableColumnModelEvent e) {
        handler.apply(U.list(e));
      }
    
  public void columnMarginChanged(javax.swing.event.ChangeEvent e) {
        handler.apply(U.list(e));
      }
    
  public void columnSelectionChanged(javax.swing.event.ListSelectionEvent e) {
        handler.apply(U.list(e));
      }
    
  public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent e) {
        handler.apply(U.list(e));
      }
    
  public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent e) {
        handler.apply(U.list(e));
      }
    
  public void popupMenuCanceled(javax.swing.event.PopupMenuEvent e) {
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
    
  public void menuSelected(javax.swing.event.MenuEvent e) {
        handler.apply(U.list(e));
      }
    
  public void menuDeselected(javax.swing.event.MenuEvent e) {
        handler.apply(U.list(e));
      }
    
  public void menuCanceled(javax.swing.event.MenuEvent e) {
        handler.apply(U.list(e));
      }
    
  public void menuKeyTyped(javax.swing.event.MenuKeyEvent e) {
        handler.apply(U.list(e));
      }
    
  public void menuKeyPressed(javax.swing.event.MenuKeyEvent e) {
        handler.apply(U.list(e));
      }
    
  public void menuKeyReleased(javax.swing.event.MenuKeyEvent e) {
        handler.apply(U.list(e));
      }
    
  public void menuDragMouseEntered(javax.swing.event.MenuDragMouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void menuDragMouseExited(javax.swing.event.MenuDragMouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void menuDragMouseDragged(javax.swing.event.MenuDragMouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void menuDragMouseReleased(javax.swing.event.MenuDragMouseEvent e) {
        handler.apply(U.list(e));
      }
    
  public void valueChanged(javax.swing.event.ListSelectionEvent e) {
        handler.apply(U.list(e));
      }
    
  public void intervalAdded(javax.swing.event.ListDataEvent e) {
        handler.apply(U.list(e));
      }
    
  public void intervalRemoved(javax.swing.event.ListDataEvent e) {
        handler.apply(U.list(e));
      }
    
  public void contentsChanged(javax.swing.event.ListDataEvent e) {
        handler.apply(U.list(e));
      }
    
  public void internalFrameOpened(javax.swing.event.InternalFrameEvent e) {
        handler.apply(U.list(e));
      }
    
  public void internalFrameClosing(javax.swing.event.InternalFrameEvent e) {
        handler.apply(U.list(e));
      }
    
  public void internalFrameClosed(javax.swing.event.InternalFrameEvent e) {
        handler.apply(U.list(e));
      }
    
  public void internalFrameIconified(javax.swing.event.InternalFrameEvent e) {
        handler.apply(U.list(e));
      }
    
  public void internalFrameDeiconified(javax.swing.event.InternalFrameEvent e) {
        handler.apply(U.list(e));
      }
    
  public void internalFrameActivated(javax.swing.event.InternalFrameEvent e) {
        handler.apply(U.list(e));
      }
    
  public void internalFrameDeactivated(javax.swing.event.InternalFrameEvent e) {
        handler.apply(U.list(e));
      }
    
  public void hyperlinkUpdate(javax.swing.event.HyperlinkEvent e) {
        handler.apply(U.list(e));
      }
    
  public void insertUpdate(javax.swing.event.DocumentEvent e) {
        handler.apply(U.list(e));
      }
    
  public void removeUpdate(javax.swing.event.DocumentEvent e) {
        handler.apply(U.list(e));
      }
    
  public void changedUpdate(javax.swing.event.DocumentEvent e) {
        handler.apply(U.list(e));
      }
    
  public void stateChanged(javax.swing.event.ChangeEvent e) {
        handler.apply(U.list(e));
      }
    
  public void editingStopped(javax.swing.event.ChangeEvent e) {
        handler.apply(U.list(e));
      }
    
  public void editingCanceled(javax.swing.event.ChangeEvent e) {
        handler.apply(U.list(e));
      }
    
  public void caretUpdate(javax.swing.event.CaretEvent e) {
        handler.apply(U.list(e));
      }
    
  public void ancestorMoved(javax.swing.event.AncestorEvent e) {
        handler.apply(U.list(e));
      }
    
  public void ancestorAdded(javax.swing.event.AncestorEvent e) {
        handler.apply(U.list(e));
      }
    
  public void ancestorRemoved(javax.swing.event.AncestorEvent e) {
        handler.apply(U.list(e));
      }
    
}

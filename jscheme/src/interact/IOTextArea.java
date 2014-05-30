package interact;

import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.io.Reader;
import java.io.Writer;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JTextArea;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;

/**
   <p>This is a TextArea that creates a Reader and Writer, that can
   be used (via getReader(), getWriter()) to interact with a scripting
   language.

   <p>What a user types into the bottom of the text area appears on
   the Reader.

   <p>Output from the PipedWriter usually appears above where the user
   is typing.  The user can type one line ahead as output occurs.

   <p>With help from R@y Tomlinson.

   <p><em>Collaborators</em>
   <ul>
   <li> OutputDocument is the Document model used by this TextArea.
   <li> IOTextAreaWriter is the Writer returned by getWriter().
   </ul>

 **/
public class IOTextArea extends JTextArea {

  private IOTextArea area = this; // Synonym for this.
  private OutputDocument doc = new OutputDocument();
  private PipedReader readEnd = null;
  private PipedWriter writeEnd = null;
  private Writer writer = null;

  public IOTextArea(int rows, int cols) {
    super(rows, cols);
    this.area.setDocument(doc);
    this.updateKeymap();
  }

  public synchronized void replaceSelection(String s) {
    super.replaceSelection(s); }

  public void write(char[] buf, int off, int len) throws IOException {
    this.write(new String(buf, off, len)); 
  }
  
  public void write(final String s) {
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
	int outPos = doc.getOutputOffset();
	int dot = area.getCaretPosition();
	doc.insertOutput(s, null);
	if (dot >= outPos) area.setCaretPosition(dot + s.length());
      }}); }
  
  protected void updateKeymap() {
    // KeyStroke key = KeyStroke.getKeyStroke('\n'); didn't work.
    KeyStroke key = KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER,
					   0,false);
    Action action = new AbstractAction() {
      public void actionPerformed(ActionEvent e) {
	((IOTextArea) e.getSource()).handleNewline(); }};
    Keymap newKeymap = JTextComponent.addKeymap
      ("IOTextAreaKeymap", this.area.getKeymap());
    newKeymap.addActionForKeyStroke(key, action);
    this.area.setKeymap(newKeymap); }

  /** has someone created a reader? **/

  protected void enableIO() {
    if (this.readEnd == null) {
      this.readEnd  = new PipedReader();
      this.writeEnd = new PipedWriter();
      try { readEnd.connect(writeEnd); }
      catch (IOException e) { e.printStackTrace(); }
    }}

  public Reader getReader() {
    enableIO();
    return this.readEnd;
    }

  protected void handleNewline() {
    enableIO();
    SwingUtilities.invokeLater(new Runnable() {
	public void run() {
	  area.setCaretPosition
	    (doc.handleNewline(writeEnd, area.getCaretPosition()));
	}}); }

  public Writer getWriter() {
    enableIO();
    if (this.writer != null) return this.writer;
    this.writer = new IOTextAreaWriter(this.area);
    return this.writer; }
      
}

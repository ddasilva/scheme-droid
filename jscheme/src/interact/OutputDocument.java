package interact;

import java.io.Writer;
import java.io.IOException;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;
import javax.swing.text.Position;

/**
   This  is a  Document model  used  by IOTextArea.   It maintains  an
   output position where output should be inserted.  The user can type
   one line ahead at the bottom of the document as output is coming out.
   <p>When the user presses return, his input is sent to a Writer.

 **/

public  class OutputDocument extends PlainDocument {
  Position outputPos = newPosition(0);

  public OutputDocument() {
    super();
  }

  /** Offset in document where next output will go. **/
  public int getOutputOffset() { return this.outputPos.getOffset(); }

  /** Returns where caret should go. **/
  public int handleNewline(Writer w, int dot) {
    int outPos = this.getOutputOffset();
    int eob = this.getLength();
    if (dot >= outPos && w != null) {
      try {
	super.insertString(eob, "\n", null);
	w.write(this.getText(outPos, eob + 1 - outPos));
	w.flush();
	this.outputPos = newPosition(eob+1); }
      catch (IOException e) { e.printStackTrace(); }
      catch (BadLocationException e) { e.printStackTrace(); }
      return eob+1;
    } else return dot;
  }

  /** Create a new Position at offset handling the annoying Exception. **/
  private Position newPosition(int offset) {
    try { return this.createPosition(offset); }
    catch (BadLocationException e) {
      e.printStackTrace();
      System.out.println("Return Position(0)!");
      return newPosition(0); }}
  /**
     This is the normal method to insert a string.  If the insertion
     point is the same as the outputPos, the outputPos will move, so
     we need to make a new one at the old offset.
  **/
  public void insertString(int offset, String text, AttributeSet as) 
      throws javax.swing.text.BadLocationException {
    int oldOffset = outputPos.getOffset();
    super.insertString(offset, text, as);
    if (oldOffset == offset) outputPos = this.newPosition(oldOffset);
  }

  /** Use this to insert text at the output Position. **/
  public synchronized void insertOutput(String text, AttributeSet as) {
    int oldOffset = outputPos.getOffset();
    int newOffset = oldOffset + text.length();
    try { super.insertString(oldOffset, text, as); } // Super!
    catch (BadLocationException e) {
      System.out.println("This shouldn't happen!");
      e.printStackTrace();
    }
    if (outputPos.getOffset() != newOffset)
      outputPos = this.newPosition(newOffset);
  }
}    

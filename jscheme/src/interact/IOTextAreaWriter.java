package interact;

import java.io.IOException;
import java.io.Writer;

/**
   A Writer that writes into the output area of an IOTextArea.
 **/

public class IOTextAreaWriter extends Writer {

  private IOTextArea area;

  public IOTextAreaWriter(IOTextArea area) { this.area = area; }

  private void ensureOpen() throws IOException {
    if (area == null) throw new IOException("Writer " + this + " closed"); }

  public void write(String s) throws IOException {
    ensureOpen();
    this.area.write(s); }

  public void write(String s, int off, int len) throws IOException {
    ensureOpen();
    if (off == 0 && len == s.length()) this.area.write(s);
    else super.write(s, off, len); }

  public synchronized void write(final char[] buf,
				 final int off, 
				 final int len) throws IOException {
    ensureOpen();
    this.area.write(buf, off, len); }

  public synchronized void flush() throws IOException {ensureOpen();}

  public synchronized void close() throws IOException {
    ensureOpen();
    area = null; }
}

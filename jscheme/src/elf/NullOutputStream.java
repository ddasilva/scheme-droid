package elf;
import java.io.*;

/** An OutputStream that doesn't output anywhere.  Stolen from Renu. */

public class NullOutputStream extends OutputStream {
  public NullOutputStream () {super();}
  public void write (byte [] b) {}
  public void write (byte [] b, int off, int len) {}
  public void write (int b) {}
}

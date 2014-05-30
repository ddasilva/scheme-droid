package jsint;
/**
 * This is the parent class for the Java Listeners.
 * There are several subclasses corresponding to each of the
 * various extensions of Java. Each extension adds a few
 * listeners.
 * @author Timothy J. Hickey, Copyright (c) 2000, tim@cs.brandeis.edu <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
*/

public class JavaListener {
  public Procedure handler;
  public JavaListener() {super();}
  public JavaListener(Procedure handler) {
    this.handler = handler;
  }
}

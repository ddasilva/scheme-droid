package using;

import jscheme.JScheme;

/*
  This is an example of how a Java program can use JScheme code.  It
  provides static methods you can use to interatively manipulate Java
  Objects.  It beats debugging with print statements and backtraces.

  <p>Much of what you need to access JScheme from Java is in the class
  jscheme.JScheme, the Java Scheme interface.
 */
public class Active {
  /** The JScheme instance we will use/ */
  private static JScheme js = new JScheme();

  /** Load the JScheme code you need when the class is first referenced. **/
  static { js.evalOrLoad("elf/basic.scm"); }

  /** Describe the contents of Object x. **/
  public static void describe (Object x) { js.call("describe", x); }

  /** Start a detatched garbage collection panel **/
  public static void gcMonitor () { js.evalOrLoad("elf/GCMonitor.scm"); }

  /** Inspect the object, x using a JTable. **/
  public static void inspect (Object x) { js.call("inspect", x); }

  /** Interact with, object it in an interactive JScheme window. **/
  public static void interact (Object it) { new interact.Interactor(it); }
}

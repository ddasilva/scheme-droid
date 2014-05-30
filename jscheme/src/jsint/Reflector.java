package jsint;

import java.util.Enumeration;
import java.util.Vector;

/** A Reflector contains one or more Java metaobjects that are cached.
    They need to be reset() when the classpath is reset.
**/


public abstract class Reflector extends Procedure {
  public static final Vector reflectors = new Vector(100);

  /** Reset all know reflectors **/
  public static void resetAll() {
    Enumeration i = reflectors.elements();
    while (i.hasMoreElements())
      ((Reflector) i.nextElement()).reset();
  }

  public boolean isPrivileged = false;

  /** Add yourself to the reflectors **/
  public Reflector() {
    reflectors.addElement(this);
  }

  /** Reset your classpath dependent state.  This method can't be
      abstract.
  **/
  protected synchronized void reset() {}

  protected Object readResolve() {
    reset();
    return this;
  }

}

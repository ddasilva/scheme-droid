package jsint;

/**
   A Procedure that can invoke a Method.
 * @author Ken R. Anderson, Copyright 2000, kanderso@bbn.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public class RawMethod extends Procedure{
  private Method method;

  public RawMethod(Method method) {
    this.method = method;
    this.minArgs = (this.isStatic() ? 0 : 1) +
      method.getParameterTypes().length;
    this.maxArgs = minArgs;
  }

  public boolean isStatic() {
    return Modifier.isStatic(method.getModifiers()); 
  }
    
  public Object apply(Object[] args) {
    if (this.isStatic()) return Invoke.invokeRawMethod(method, null, args);
    else {
      int L = this.minArgs - 1;
      Object[] newArgs = new Object[L];
      System.arraycopy(args, 1, newArgs, 0, L);
      return Invoke.invokeRawMethod(method, args[0], newArgs);
    }
  }
}

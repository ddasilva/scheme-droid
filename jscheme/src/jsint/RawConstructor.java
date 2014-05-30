package jsint;

/**
   A procedure that invokes a Constructor.
 * @author Ken R. Anderson, Copyright 2000, kanderso@bbn.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */

import java.lang.reflect.Constructor;

public class RawConstructor extends Procedure{
  private Constructor method;

  public RawConstructor(Constructor method) {
    this.method = method;
    this.minArgs = method.getParameterTypes().length;
    this.maxArgs = minArgs;
  }
    
  public Object apply(Object[] args) {
    return Invoke.invokeRawConstructor(method, args);
  }
}

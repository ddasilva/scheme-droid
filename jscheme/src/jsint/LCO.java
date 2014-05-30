package jsint;

/**
 * A aupport class for the Scheme->Java compiler
 * used to implement the last call optimization
 * @author Timothy J. Hickey, Copyright (c) 2000, tim@cs.brandeis.edu <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */

public class LCO
{
  public Object func;
  public Pair params;

  public LCO(Object func, Pair params)
  {
    this.func = func;
    this.params = params;
  }

  public static Object eval(Object X)
    {
	while( X instanceof LCO) {
	    Object f = ((LCO) X).func;
	    if (f instanceof Function)
		X = ((Function)f).invoke1(((LCO)X).params);
            else if (f instanceof Procedure)
		X = ((Procedure)f).apply(((LCO)X).params);
	}
	return X;
    }
}

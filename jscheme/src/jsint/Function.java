package jsint;

/**
 * A support class for the Scheme->Java compiler 
 * @author Timothy J. Hickey, Copyright (c) 2000, tim@cs.brandeis.edu <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */
public interface Function {
  public Object invoke(Pair args);
  public Object invoke1(Pair args);
}

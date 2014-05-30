package jsint;

/** A LocalVariable is denoted by its position in the environment, in terms
 * of the number of levels "up" we have to go (number of nested environments),
 * the number of variables "in" we have to go (ordinal position of variable),
 * and whether the variable is a "rest" (or "dotted") variable.
 * Example of variables in <tt>(list x y z a b)</tt>:
 * <fmt>
 * (lambda (x y)                x.up=2, x.in=0    y.up=2, y.in=1
 *   (lambda (z . a)            z.up=1, z.in=0    a.up=1, a.in=1
 *      (lambda b               b.up=0, b.in=0
 *        (list x y z a b))))
 * </fmt>
 * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 **/
public class LocalVariable implements java.io.Serializable {

  /** The number of levels up in the parent chain where the variable is. **/
  public int up;
  /** The ordinal position in the environment where the variable is. **/
  public int in;
  /** The name of the variable (for debugging purposes only). **/
  Symbol name;

  /** Create a new variable. **/
  public LocalVariable(int up, int in, Symbol name) {
    this.up = up; this.in = in; this.name = name;
  }

  /** Use the name, up and in of the variable as its String
      representation so they distinguish themselves from a Global
      variable. **/
  public String toString() { return name.toString() + "^" + up + in; }
}

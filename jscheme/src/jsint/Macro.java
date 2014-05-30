package jsint;

/**
   A macro.

  @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
  subsequently modified by Jscheme project members
  licensed under zlib licence (see license.txt)
**/

public class Macro extends Closure {

    /** Make a macro from a parameter list, body, and environment. **/
    public Macro (Object parms, Object body, LexicalEnvironment lexenv) {
      super(parms, body, lexenv);
    }


  public static Object expand(Pair x) {
    Object f = U.first(x);
    Object fval = (f instanceof Symbol) ? 
                       (((Symbol) f).isDefined()?((Symbol)f).getGlobalValue():null)
                       : null;
    if (fval instanceof Macro)
      return ((Macro)fval).apply(U.toList(x.rest));
    else return x;
  }

  public String toString() {
    return "(macro " + this.name + " " + U.stringify(parms) + "...)";
  }
}

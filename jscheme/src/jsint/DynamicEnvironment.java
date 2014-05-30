package jsint;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.HashSet;

/**
   A Map from Symbol to DynamicVariables.
 **/

public class DynamicEnvironment implements java.io.Serializable {
  /** This is here for historical reasons. **/
  public static boolean showJavaDotWarnings = false;
  /** Is this locked down so it can't be changed? **/
  private boolean lockedDown = false;
  protected Hashtable rep;

  public DynamicEnvironment() {
    rep = new Hashtable(100);
  }

  public DynamicEnvironment(DynamicEnvironment baseEnv) {
    rep = new Hashtable(baseEnv.rep.size()+100);
    Enumeration keys = baseEnv.rep.keys();
    while (keys.hasMoreElements()) {
      Object k = keys.nextElement();
      rep.put(k, baseEnv.rep.get(k));
    }
  }

  public void lockDown() {
    lockedDown = true;
  }

  /** This is used by the (environment-bindings) primitive. **/
  public static Pair getBindings(Object x) {
    if (! (x instanceof DynamicEnvironment)) {
      E.error("ERROR: not an environment: "+x.toString());
      return null;
    }
    else {
      Pair bindings = Pair.EMPTY;
      DynamicEnvironment env = (DynamicEnvironment) x;
      Enumeration keys = env.rep.keys();
      while (keys.hasMoreElements()) {
        Symbol k = (Symbol) keys.nextElement();
        if (! isJavaLiteral(k) && env.isDefined(k)) {
          // Only export non-Java-literals.
          Object v = env.getValue(k);
          bindings = new Pair(new Pair(k, v), bindings);
        }
      }
      return bindings;
    }
  }

  /** KRA 16JUL04: This is just a guess right? **/
  private static boolean isJavaLiteral(Symbol name) {
    return (name.toString().indexOf('.') != -1);
  }


  public void importBindings(DynamicEnvironment env, String prefix)
    {importBindings(env, prefix, false);}

  public void importBindings(DynamicEnvironment env,
			     String prefix,
			     boolean importMacros)
    {importBindings(env, prefix, importMacros, null);}

  public void importBindings(DynamicEnvironment env,
			     String prefix,
			     boolean importMacros,
			     jsint.Symbol[] procnames) {
    HashSet hs = null;

    if (procnames!=null)
      hs = new HashSet(java.util.Arrays.asList(procnames));
    if (lockedDown)
      E.error("ERROR: attempting to import bindings into locked environment");
    else {
      Enumeration keys = env.rep.keys();
      while (keys.hasMoreElements()) {
        Symbol k = (Symbol) keys.nextElement();
        // Don't copy java literals, since we look them up when needed.
        if (env.isDefined(k) && (! isJavaLiteral(k))) {
          Object v = env.getValue((Symbol) k);
          if ( (importMacros == (v instanceof Macro)) && 
               ((hs==null) || hs.contains(k))   ) {
            Symbol newk = (prefix!=null ?
                           Symbol.intern(prefix+k.toString()) : k);
            setValue(newk, v);
          }}}}}

  public Object getValue(Symbol s) {
    return intern(s).getDynamicValue();
  }

  public boolean isDefined(Symbol s) {
    DynamicVariable it = ((DynamicVariable) rep.get(s));
    return it != null && it.isDefined();
  }

  public Object setValue(Symbol s, Object newval) {
    if (lockedDown)
      return E.error
        ("ERROR: attempting to alter bindings in locked environment:"+
         s.toString()+" <-- "+newval.toString());
    return intern(s).setDynamicValue(newval);
  }

  public DynamicVariable intern(Symbol x) {
    DynamicVariable it = ((DynamicVariable) rep.get(x));
    if (it == null) {
      it = new DynamicVariable(x);
      rep.put(x, it);
    }
    return it;
  }
}

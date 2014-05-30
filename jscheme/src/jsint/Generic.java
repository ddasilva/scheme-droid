package jsint;

/**  
     A generic function.

 * @author Ken R. Anderson, Copyright 2000, kanderso@bbn.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */

public class Generic extends StaticReflector {

  private Object[] methodTable;
  public Object[] getMethodTable() { return methodTable; }

  public Generic(String name) {
    this.name = name;
  }

  private void addMethod(Class[] types, Procedure p) {
    if (methodTable == null) {
      methodTable = new Object[Invoke.BUCKET_SIZE];
      install(0, types, p);
    } else {
      for(int i = 0; i < methodTable.length; i = i + Invoke.BUCKET_SIZE)
	if (Invoke.parameterTypesMatch(((Class[]) methodTable[i]), types)) {
	  install(i, types, p);
	  return;
	}
      grow();
      install(methodTable.length - Invoke.BUCKET_SIZE, types, p);
    }
  }

  private void install(int i, Class[] types, Procedure p) {
    methodTable[i] = types;
    methodTable[i+1] = p;
  }

  private void grow() {
    Object[] ms = new Object[methodTable.length + Invoke.BUCKET_SIZE];
    System.arraycopy(methodTable, 0, ms, 0, methodTable.length);
    methodTable = ms;
  }

  private static Generic ensureGeneric(Symbol name) {
    if (name.isDefined()) {
      Object v = name.getGlobalValue();
      if (! (v instanceof Generic)) {
	E.warn(name + " was bound to " + v + " it is now a Generic.");
	Generic g = new Generic(name.toString());
	name.setGlobalValue(g);
	return g;
      } else return ((Generic) v);
    } else {
      Generic g = new Generic(name.toString());
      name.setGlobalValue(g);
      return g;
    }}

  public static synchronized Generic defineMethod(Symbol name,
                                                  Pair types,
                                                  Procedure p) {
    Generic g = ensureGeneric(name);
    g.addMethod(Invoke.toClassArray(types, 0), p);
    return g;
  }

  public Closure findMethod(Object[] args) {
    return ((Closure) Invoke.findMethodNoOpt(methodTable, args));
  }

  public Object apply(Object[] args) {
    Procedure m = ((Procedure) Invoke.findMethodNoOpt(methodTable, args));
    // return ((Procedure)m).apply(args);
    // KRA 11FEB02: Quick hack to get compiling working again.
    if (m.minArgs == 0 && m.maxArgs == Integer.MAX_VALUE) 
      return m.apply(new Object[] { U.vectorToList(args) });
    else return m.apply(args);
  }

  protected synchronized void reset() {
    for (int i = 0; i < methodTable.length; i = i + Invoke.BUCKET_SIZE)
      methodTable[i] = resetParameters((Class[]) methodTable[i]);
  }

  private Object[] resetParameters(Class[] parameters) {
    Class[] parameters2 = new Class[parameters.length];
    for (int i = 0; i < parameters2.length; i++)
      parameters2[i] = Import.forName(parameters[i].getName());
    return parameters2;
  }
}

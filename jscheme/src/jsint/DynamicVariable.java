package jsint;

/** A DynamicVariable represents a global value in a particular
 * environment. **/

public class DynamicVariable implements java.io.Serializable {

  Symbol name;
  Object value = U.UNDEFINED;

    public DynamicVariable(Symbol name) {
      this.name = name;
    }

  public boolean isDefined() {
    return value != U.UNDEFINED;
  }

  public Object getDynamicValue() {
    Object value = this.value;
    /** You may be naming a Java dot item but not have a value yet. **/
    if (value == U.UNDEFINED) {
      value = lookupJavaBinding(name);
      if (value != null) this.value = value;
    }
    if (value == U.UNDEFINED)
      return E.error("ERROR: undefined variable \"" + name +"\"");
    else if ((value instanceof JavaField) && ((JavaField) value).isStatic)
      return ((JavaField) value).apply(U.NO_ARGS);
    else return value;
  }

  public Object setDynamicValue(Object newval) {
    /** Look at the existing value to see if it is static field we
     * should set. **/
    Object existingValue = this.value;
    if (existingValue == U.UNDEFINED) {
      Object tmpValue = lookupJavaBinding(name);
      if (tmpValue != null) {
	this.value = tmpValue;
	existingValue = tmpValue;
      }
    }
    /** If you are a static field set the value there. **/
    if ((existingValue instanceof JavaField) &&
	((JavaField) existingValue).isStatic)
      ((JavaField) existingValue).apply(new Object[]{ newval });
    else this.value = newval;
    return newval;
  }

  /** Look like a symbol so (describe) can show procedures easily **/
  public String toString() {
    return name.toString();
  }

  private void writeObject(java.io.ObjectOutputStream out)
    throws java.io.IOException {
    out.writeObject(name);
    /** Toby Allsop: Use value rather than getDynamicValue() so you
	can serialize code that has java dot of a class that doesn't
	exist yet. **/
    out.writeObject(value);
  }

  private void readObject(java.io.ObjectInputStream in)
    throws java.io.IOException, ClassNotFoundException {
    name = (Symbol) in.readObject();
    value = in.readObject();
  }

  /** When deserialized, add yourself to the interaction environment
   *  and give it a value if you have one.
   **/
  private Object readResolve() throws java.io.ObjectStreamException {
    DynamicVariable it = Scheme.getInteractionEnvironment().intern(name);
    if (value != U.UNDEFINED) it.value = value;
    return it;
  }

  /** If var is a Java Literal, then return its value as a
      method/constructor/field object otherwise return null **/
  private static  Object lookupJavaBinding(Symbol var) {
    String fullname = var.toString();
    String name = fullname;
    boolean canAccessPrivateData = false;
    if (fullname.endsWith("#"))
      {canAccessPrivateData=true;
      name=fullname.substring(0,name.length()-1);}
    int firstIndex = name.indexOf('.');
    int lastIndex = name.lastIndexOf('.');
    int fieldIndex = name.lastIndexOf('$');
    int nameLength=name.length();

    Object value=U.UNDEFINED;

    if (firstIndex == -1) {
      return value;  // symbol is not a Java Literal
    }
    try{
      if (fieldIndex == nameLength-1) {
	if (firstIndex == 0) {
	  if (lastIndex > 0){  // ".CLASS.NAME$"
	    value=
	      (new JavaField(name.substring(lastIndex+1,nameLength-1),
			     Import.classNamed(name.substring(1,lastIndex)),
                             canAccessPrivateData));
	  } else {             // ".NAME$"
	    value=
	      (new JavaField(name.substring(1,nameLength-1),null,canAccessPrivateData));
	  }
	}
        else {                 // "CLASS.NAME$"
	  value=
	    (new JavaField(name.substring(lastIndex+1,nameLength-1),
			   Import.classNamed(name.substring(0,lastIndex)),
                           canAccessPrivateData));

	}
      }else if (firstIndex == 0) {
	if (nameLength==1) return var;                    // "."
	else if (lastIndex > 0)                           // ".CLASS.NAME"
	  value=  (
		   new JavaMethod(
				  name.substring(lastIndex+1,nameLength),
				  Import.classNamed(name.substring(1,lastIndex)),
				  false,
                                  canAccessPrivateData));
	else value=
	       (new JavaMethod
		(name.substring(1,nameLength),null,false,canAccessPrivateData));
      }
      else if (lastIndex==nameLength-1)	                // "CLASS."
	value=  (new JavaConstructor(Import.classNamed
				     (name.substring(0,lastIndex)).getName(),
                                     canAccessPrivateData));
      else {
	String methodName = name.substring(lastIndex+1,nameLength);
        if ("class".equals(methodName))                 // "CLASS.class"   class literal
	  value=  (Import.classNamed(name.substring(0,lastIndex)));
        else
	  value=                           // "CLASS.NAME"
	    (new JavaMethod(name.substring(lastIndex+1,nameLength),
			    Import.classNamed(name.substring(0,lastIndex)),true,canAccessPrivateData));
      }
      return value;
    } catch (Exception e) {
      if (DynamicEnvironment.showJavaDotWarnings)
	Scheme.currentEvaluator().getError().
	  println("Javadot WARNING: " + e.getMessage());

      return value;
    }
  }
}

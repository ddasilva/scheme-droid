 package dclass;
import jsint.Procedure;
/** The metaclass Class. **/
public class Class extends Modified {
  public Object extending;
  public Object implementing;
  public boolean isInterface;
  public Object clauses;

  public Class (Object modifiers, boolean isInterface, Object name,
		Object extending, Object implementing) {
    this .modifiers   = modifiers;
    this.isInterface  = isInterface;
    this.name         = name;
    this.extending    = extending;
    this.implementing = implementing;
  }

  public String toString() {
    return "{" + modifiers + " " +
      ((isInterface) ? " interface " : " class ") + " " +
      name + " " +
      extending + " " +
      implementing + "}";
  }

}

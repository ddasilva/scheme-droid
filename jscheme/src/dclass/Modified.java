 package dclass;
import jsint.Procedure;
public class Modified {
  public Object modifiers;
  public Object name;
  public String toString() {
    return "{" + this.getClass().getName() + " " + modifiers + " "
      + name + "}";
  }
}

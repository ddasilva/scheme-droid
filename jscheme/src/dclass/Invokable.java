// constructor ::= modifier*      name args* body*
package dclass;

public class Invokable extends Modified {
  public Object args;
  public Object body;

  public String toString() {
    return "{" + this.getClass().getName() + " " + modifiers + " "
      + name + " " + args + " " + body + " "
      + "}";
  }
}

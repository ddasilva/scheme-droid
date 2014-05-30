package dclass;
public class Method extends Invokable {
  public Object type;

  public Method (Object modifiers, Object type, Object name,
		 Object args, Object body) {
    this.modifiers = modifiers;
    this.type = type;
    this.name = name;
    this.args = args;
    this.body = body;
  }

  public String toString() {
    return "{" + this.getClass().getName() + " " + modifiers + " "
      + type + " " + name + " " + args + " " + body + " " + "}";
  }
}

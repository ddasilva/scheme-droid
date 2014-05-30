package dclass;

/** The metaclass Constructor. **/
public class Constructor extends Invokable {
  public Constructor (Object modifiers, Object name,
		      Object args, Object body) {
    this.modifiers = modifiers;
    this.name = name;
    this.args = args;
    this.body = body;
  }
}

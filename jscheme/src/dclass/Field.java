package dclass;
public class Field extends Modified {
  public static final Object NOVALUE = new Object[] {"noValue"};
  public Object type;
  public Object value = NOVALUE;

  public Field () {
    super();
  }

  public Field(Object modifiers, Object type, Object name) {
    this.modifiers = modifiers;
    this.type = type;
    this.name = name;
  }

  public Field(Object modifiers, Object type, Object name, Object value) {
    this(modifiers, type, name);
    this.value = value;
  }

  public boolean hasValue() { return value == NOVALUE; }

  public String toString() {
    return "{" + this.getClass().getName() + " " + modifiers + " "
      + type + " " + name + " " +  "}";
  }
}

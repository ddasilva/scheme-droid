package jsint;
import java.util.Hashtable;

/** In Jscheme 1.0 to 1.4, symbols were implemented as Strings.  In 1.5, we
 * add a Symbol class, with a field for the global value. This makes
 * global lookup faster, but limits us to only one global environment.
 * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 **/

public class Symbol implements java.io.Serializable, jscheme.SchemeSymbol {

  private String name;
  
  /** The symbolTable keeps a unique mapping from String name to Symbol. **/
  public static final Hashtable symbolTable = new Hashtable(500);

  public static final Symbol 
    BEGIN            = intern("begin"),
    CAR              = intern("car"),
    CDR              = intern("cdr"),
    DEFINE           = intern("define"),
    IF               = intern("if"),
    LAMBDA           = intern("lambda"),
    MACRO            = intern("macro"),
    NEWLINE          = intern("newline"),
    NULL             = intern("null"),
    OR               = intern("or"),
    QUASIQUOTE       = intern("quasiquote"), 
    QUOTE            = intern("quote"),
    SET              = intern("set!"),
    SPACE            = intern("space"),
    UNQUOTE          = intern("unquote"),
    UNQUOTE_SPLICING = intern("unquote-splicing"),
    PACKAGE          = intern("package");

  /** Constructor.  From outside the class, call Symbol.intern(name). **/ 
  private Symbol(String name) { this.name = name; } 

  private Object readResolve() throws java.io.ObjectStreamException {
    return intern(name);
  }

  /** Intern a string: look up or make a symbol. **/
  public static synchronized Symbol intern(String name) { 
    Symbol result = (Symbol)symbolTable.get(name);
    if (result == null) symbolTable.put(name, result = new Symbol(name));
    return result;
  }

  /** A symbol can be printed using its name.  
   * We don't do escape processing. **/
  public String toString() { return name; }

  public Object getGlobalValue(){
    return Scheme.getInteractionEnvironment().getValue(this);
  }

  public synchronized Object setGlobalValue(Object newval) {
    return Scheme.getInteractionEnvironment().setValue(this, newval);
  }

  public boolean isDefined() {
    return Scheme.getInteractionEnvironment().isDefined(this);
  }

}

package jsint;

/** Used by Import.  One for each (import) expression. **/
public interface Importer {
  public Class classNamed(String name);
  public void reset();
}


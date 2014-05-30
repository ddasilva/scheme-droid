package build;

public abstract class Loadlet {
  protected Loadlet parent;
  public Loadlet getParent() { return this.parent; }

  Loadlet(Loadlet parent) { this.parent = parent; }

  public  byte[] findClass(String name) {
    Loadlet ls = this;
    while (ls != null) {
      byte[] bs = ls.loadClassData(name);
      if (bs != null) return bs;
      ls = ls.getParent(); }
    return null; }

  public abstract byte[] loadClassData(String name);
}

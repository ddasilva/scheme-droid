package build;

public abstract class LoadletClassLoader extends ClassLoader {

  protected Loadlet loadlet;
  protected Loadlet getLoadlet() { return this.loadlet; }
    
  public LoadletClassLoader(ClassLoader parent, Loadlet loadlet) {
    super(parent);
    this.loadlet = loadlet; }

  protected Class findClass(String name) throws ClassNotFoundException {
    byte[] b = loadlet.findClass(name);
    if (b == null) throw new ClassNotFoundException(name);
    return defineClass(name, b, 0, b.length);
  }
}

package build;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;


public class CompilingLoadlet extends Loadlet {

  // KRA 28SEP99: This is a bit of a kludge, 
  // CompilingLoadlet's can't operate independently.
  // Each must contribute to the classpath.  This is done in construction.
  private static String classPath = System.getProperty("java.class.path");
  
  private static String getClassPath() {
    synchronized (CompilingLoadlet.class) { return classPath; }}

  private static void addClassPath(String cp) {
    synchronized (CompilingLoadlet.class) {
      if (classPath == null) classPath = cp;
      else classPath = classPath + System.getProperty("path.separator") + cp; 
    }}

  protected String classBase;
  protected String srcBase;

  CompilingLoadlet(Loadlet parent, String classBase, String srcBase) {
    super(parent);
    this.classBase = classBase;
    this.srcBase = srcBase; 
    addClassPath(classBase);
  }

  /** Rules for deciding how to come up with the byte[] of a class
      named <tt>name</tt>. **/
  public byte[] loadClassData(String name) {
    File src = this.srcFile(name);
    File bin = this.binFile(name);
    boolean se = src.exists();
    boolean be = bin.exists();
    if ( be &&  se && src.lastModified() > bin.lastModified()) {
      this.compileClass(name);
      return toBytes(bin); } 
    else if ( be &&  se) return toBytes(bin);
    else if ( be && !se) return toBytes(bin);
    else if (!be &&  se) { 
      this.compileClass(name);
      return toBytes(bin); } 
    else if (!be && !se && 
	     (name.endsWith("_Skel") || name.endsWith("_Stub"))) {
      this.rmicClass(name.substring(0, name.lastIndexOf('_')));
      return toBytes(bin); }
    else return null; }

  public String toString() {
    return "new CompilingLoadLet(\"" + this.classBase + "\", \""
      + this.srcBase + "\")"; }

  public File srcFile(String name) {
    return toFile(this.srcBase, name, ".java"); }

  public File binFile(String name) {
    return toFile(this.classBase, name, ".class"); }

  public boolean compileClass(String name) {
    this.p("Compiling " + name);
    File file = this.srcFile(name);
    String[] args = new String[] { "-g", "-deprecation",
				    "-classpath", getClassPath(),
				    "-d", this.classBase,
				    "-sourcepath", this.srcBase,
				    file.toString() };
    return this.javac(args); }

  public boolean rmicClass (String name) {
    this.p("rmicing " + name);
    String[] args = new String[] { "-classpath",  getClassPath(),
				     "-d", this.classBase,
				     "-sourcepath", this.srcBase,
				     name };
    return this.rmic(args); }

  private static void p(String arg) { System.out.println(arg); }

  public static boolean javac(String[] args) {
    return new sun.tools.javac.Main(System.out, "Build-javac")
      .compile(args); }

  public static boolean jar(String[] args) {
    return new sun.tools.jar.Main(System.out, System.err, "Build-jar")
      .run(args); }

  public static boolean rmic(String[] args) {
    return new sun.rmi.rmic.Main(System.out, "Build-rmic")
      .compile(args); }

  public static File toFile(String prefix, String name, String suffix) {
    StringBuffer b = new StringBuffer(prefix.length() + name.length() +
				      suffix.length() + 1);
    char separator = '/';
    b.append(prefix);
    b.append(separator);
    for(int i = 0; i < name.length(); i++) {
      char c = name.charAt(i);
      if (c == '.') b.append(separator);
      else b.append(c); }
    b.append(suffix);
    return new File(b.toString()); }

  public static byte[] toBytes(File f) {
    if (f.exists()) {
      FileInputStream s = null;
      try {
	s = new FileInputStream(f);
	int L = (int) f.length();
	return toBytes(s, new byte[L]);
      } catch (FileNotFoundException e) {
	p("toBytes can't open existing file: " + f);
	e.printStackTrace();
	return null;
      } finally {
	if (s != null)
	  try {
	    s.close();
	  } catch (java.io.IOException e) { e.printStackTrace(); }}}
    else return null; }
    
  public static byte[] toBytes(InputStream s, byte[] b) {
    int L = b.length;
    int off = 0;
    int in = L;
    try {
      while (L > 0) {
	in = s.read(b, off, L);
	off = off + in;
	L = L - in;
      }} catch (IOException e) {
	return null; }
    return b; }
}  

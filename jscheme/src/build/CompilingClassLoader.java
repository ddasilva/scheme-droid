package build;

import java.lang.reflect.Method;
import java.util.Iterator;

/**

   A ClassLoader that will recompile the *.java file corresponding to
   an out of date or missing *.class file.  It also runs rmic to
   generate *_Stub.class and *_Skel.class files used by RMI.

   <p>
   A single CompilingClassLoader uses a sequence of pairs of
   directories:

   <ul>
    <li>classBase - where to look for *.class files.
    <li>srcBase   - where to look for *.java files.
   </ul>

   The -Dbootstrap.path System property can contain a sequence of
   classBase,srcBase pairs separated by ",".  The default is
   "-Dbootstrap.path=.,.".  For each class,src pair, a new
   CompilingClassLoader is constructed and chained to previous ones.
   <p>
   
   <h3>Sample Usage:</h3>

   Supose we have a Java application, app, with the following files and
   classes:
   <ul>
    <li> app.Main          - Main class of the application.
    <li> app/classlib/     - class file directory.
    <li> app/src/          - java file directory.
    <li> app/build/        - where this file is
    <li> jdk/lib/tools.jar - JDK *.jar file containing javac.
   </ul>

   The to compile and run the application, do:

   <pre>
   javac -classpath app;jdk/lib/tools.jar -d app
         app/build/CompilingClassLoader.java

   java -Dbootstrap.path=app/classlib,app/src 
        -classpath app;jdk/lib/tools.jar
	CompilingClassLoader app.Main
   </pre>

   The -classpath must provide access to CompilingClassLoader and
   tools.jar.  It should not contain any directories for classes that
   you want recompiled.  Specify them in -Dbootstrap.path instead.

   <p> When getting a class by name, use
   CompilingClassLoader.forName() rather than Class.forName().  This
   will recompile such classes as necessary.

 **/
public class CompilingClassLoader extends LoadletClassLoader {

  public CompilingClassLoader(ClassLoader parent, Loadlet loadlet) {
    super(parent, loadlet); }

  private static Iterator crack(final String s, final char separator) {
    return new Iterator() {
	int start = 0;

	public boolean hasNext() { return start < s.length(); }

	public Object next() {
	  int end = s.indexOf(separator, start);
	  end = (end == -1) ? s.length() : end;
	  String result = s.substring(start, end);
	  start = end + 1;
	  return result; }

	public void remove() {}}; }

  /**
     Start your Java application through this main() as described
     above to get automatic compiling behavior.
  **/
  public static void main(String[] args) {
    try {
      Iterator paths = crack(System.getProperty("bootstrap.path", ".,."), ',');
      Loadlet loadlet = null; 
      while (paths.hasNext()) {
	String bin = (String) paths.next();
	if (paths.hasNext()) {
	  String src = (String) paths.next();
	  loadlet = new CompilingLoadlet(loadlet, bin, src);
	} else break;
      }
      ClassLoader loader = new CompilingClassLoader
	(CompilingClassLoader.class.getClassLoader(), loadlet);
      Class c = loader.loadClass(args[0]);
      Class[] types = new Class[] { String[].class};
      Method m = c.getMethod("main", types);
      int n = 1;
      String[] newArgs = new String[args.length - n];
      for (int i = 0; i < newArgs.length; i++)
	newArgs[i] = args[i+n];
      Object[] parameters = new Object[] { newArgs };
      m.invoke(null, parameters);
    } catch (Exception e) {
      e.printStackTrace(); }}
}

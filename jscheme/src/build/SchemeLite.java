package build;

import java.net.URL;
/**
   Starts Jscheme from its standard URL.  This is the only
   class you really need to run Jscheme.
**/
public class SchemeLite {
 
  /** Invoke jscheme.REPL.main() from sourceforge. **/
  public static void main(String[] args) {try {
    new java.net.URLClassLoader
      (new URL[] {
	new URL(System.getProperty
		("build.SchemeLite.url",
		 "http://jscheme.sourceforge.net/jscheme/lib/jscheme.jar"))})
      .loadClass("jscheme.REPL")
      .getMethod("main", new Class[]  { String[].class })
      .invoke(null, new Object[] { args });
  } catch (Exception e) { e.printStackTrace(); }}}

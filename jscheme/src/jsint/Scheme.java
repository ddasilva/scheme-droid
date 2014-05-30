package jsint;
import java.io.*;
import java.net.URL;
import java.util.LinkedList;
import java.util.NoSuchElementException;
/** This class represents a Scheme interpreter.
 * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 **/

public class Scheme {
  /** The main() arguments **/
  public static String[] ARGS;
  
  public static boolean isInterruptable() {
    return currentEvaluator().INTERRUPTABLE;
  }

  /** Maybe interrupt this thread of execution. **/
  public static void interruptCheck() {
    currentEvaluator().interruptCheck();
  }

  private static ThreadLocal currentEvaluatorStack =
    new InheritableThreadLocal() {
      protected Object initialValue() {
        return new LinkedList();
      }
      protected Object childValue(Object parentValue) {
        return new LinkedList((LinkedList) parentValue);
      }
  };

  private static LinkedList evaluatorStack() {
    return (LinkedList) currentEvaluatorStack.get();
  }

  public static Evaluator currentEvaluator() {
    try {
      return (Evaluator) evaluatorStack().getFirst();
    } catch (NoSuchElementException e) {
      /** Use the first evaluator. **/
      pushEvaluator(jscheme.JS.js.getEvaluator());
      return (Evaluator) evaluatorStack().getFirst();
    }
  }

  public static void pushEvaluator(Evaluator i) {
    evaluatorStack().addFirst(i);

  }

  public static Evaluator popEvaluator() {
    return (Evaluator) evaluatorStack().removeFirst();
  }

  public static DynamicEnvironment getInteractionEnvironment() {
    return currentEvaluator().interactionEnvironment;
  }

  public static DynamicEnvironment getNullEnvironment() {
    return Evaluator.NULL_ENVIRONMENT;
  }

  public static DynamicEnvironment getInitialEnvironment() {
    return currentEvaluator().INITIAL_ENVIRONMENT;
  }

  /**
     The following steps are performed
     <ul>

     <li> If an "init.scm" resource or file is found, it is loaded.
     It is expected to do all the processing, for example interpreting
     command line arguments.

     <li> Otherwise command line arguments are processed as follows:

     <ul> <li> If the argument "-main" is seen, the next argument is
     either "none" indicating that no main will be started, or the
     name of a Scheme procedure or Java reflector to be invoked.  All
     of the remaining arguments are passed to it as a
     <tt>String[]</tt>.
     
     <li> An argument beginning with "(" is read and evaluated as a
     Scheme expression.

     <li> Otherwise, the argument is treated as a file, resource or URL to
      be loaded. </ul></ul>

      <p>By putting the init.scm in a .jar file with the manifest
      entry:

      <pre> Main-Class: jsint.Scheme </pre>
      you get scriptable jar files that can be invoked by:
      <pre> java -jar name.jar</pre>

      This idea was suggested by David May <a
      href=mailto:david@davudsplace.net>david@davudsplace.net</a>.

      <p>By adding command line arguments or providing your own
      init.scm file you can override the init.scm file in the .jar to
      tailor your application.
  **/

  public static void main(String[] files) {
    ARGS = files;
      if (!loadInit()) defaultMain(files);
  }

  /** The default main() behavior. **/
  public static void defaultMain(String[] files) {
    String main = null;
    String[] mainArgs = null;
    int i = 0;
    while (i < (files == null ? 0 : files.length)) {
      if (files[i].startsWith("("))
	load(new InputPort(new StringReader(files[i])));
      else if (files[i].startsWith("-")) {
	if (files[i].equals("-s")) U.useJavaSyntax=false;
	else if (files[i].equals("-j")) U.useJavaSyntax=true;
	else if (files[i].equals("-main")) {
	  i = i + 1;
	  main = files[i];
	  mainArgs = consumeArgs(files, i+1);
	  break;
	} else usage(files[i]);
      } else {
          load(files[i]);
      }
      i = i + 1;
    }
    if (main == null) runJscheme();
    else if (! main.equals("none"))
      // try { new jscheme.JScheme(currentEvaluator()).call(main, mainArgs); }
      try {jscheme.JS.call(main, mainArgs); }
    catch (Throwable e) {
     e.printStackTrace();
     System.exit(1);
    }
  }

  private static void usage(String arg) {
    E.warn("Unrecognized flag: " + arg + "\n" +
	   "Usage: \n" +
	   "  java jsint.Scheme [-s][-j] [(s-expr)] [file] ... [-main procedure arg1 ...]\n\n" +
	   "Where: \n" +
	   "  -s   \n" +
	   "    Use normal Scheme syntax for numbers and characters.\n\n" +
	   "  -j   \n" +
	   "    Use Javalike syntax for numbers and characters.\n\n" +
	   "  (s-exp)  \n" +
	   "    An argument that begins with \"(\" is evaluated.\n\n" +
	   "  -main procedure arg1 ...\n" +
	   "    Rather than starting the normal Scheme.main, \n" +
	   "    collect arg1 ... into a String[] and apply procedure to it.\n" +
	   "    If procedure is none, no main is started.\n"
	   );
    System.exit(1); }

  private static String[] consumeArgs(String[] files, int i) {
    String[] result = new String[files.length - i];
    int j = 0;
    while (i < files.length) result[j++] = files[i++];
    return result;
  }

  public static void runJscheme() {
    currentEvaluator().runJscheme();
  }

  /** Prompt, read, eval, and write the result.
   * Also sets up a catch for any RuntimeExceptions encountered. **/
  public static void readEvalWriteLoop(String prompt) {
    currentEvaluator().readEvalWriteLoop(prompt);
  }

  public static boolean loadInit() {
    InputPort in = open("init.scm");
    if (in != null) {
      load(in);
      return true;
    } else return false;
  }

  /** Eval all the expressions in a file. Calls load(InputPort). **/
  public static Object load(Object fileName) {
    String name = fileName.toString();
    InputPort iport = open(name);
    if (iport == null) return E.warn("(load) can't open \"" + fileName + "\"");
    else return load(iport);
  }

    /**
     * load the current file (or class) into a new initial environment
     * and return the resulting DynamicEnvironment.
     */
  public static DynamicEnvironment loadEnvironment(Object x)
  {
    return currentEvaluator().loadEnvironment(x);
  }

  public static Boolean environmentImport(Object x, Object prefix)
  {
    return currentEvaluator().environmentImport(x, prefix);
  }

  public static Boolean languageImport(Object x)
  {
    return currentEvaluator().languageImport(x);
  }

  public static InputPort open(String name) {
    InputPort ip = null;
    if (ip == null) ip = openFile(name);
    if (ip == null) ip = openResource(name);
    if (ip == null) ip = openURL(name);
    return ip;
  }

  public static InputPort openURL (String url) {
    try {
    return new InputPort((InputStream) (new URL(url)).getContent());
    } catch (java.net.MalformedURLException e) {
      return null; 
    } catch (IOException e) {
      e.printStackTrace();
      return null;
    }
  }

  public static InputPort openFile(String name) {
    try { return new InputPort(new FileInputStream(name)); }
    catch (java.io.IOException fnf) { return null; }
    catch (SecurityException se) { return null; }
    catch (Throwable e) {
      e.printStackTrace(Scheme.currentEvaluator().getError());
      return null;
    }
  }

  public static InputPort openResource(String name) {
    try {
      ClassLoader loader = Import.getClassLoader();
      InputStream stream = (loader == null) ?
	ClassLoader.getSystemResourceAsStream(name) :
	loader.getResourceAsStream(name);
      return (stream == null) ? null : new InputPort(stream);
    } catch (Throwable e) {

      Scheme.currentEvaluator().getError().
	println("In openResource(" + name + "):");
      e.printStackTrace(Scheme.currentEvaluator().getError());
      return null;
    }
  }

  public static Object load(InputPort in) {
    return currentEvaluator().load(in);
  }

  /** evalToplevel evaluates each element of a BEGIN.  This is so
      macros can be defined and then used.  Also toplevel macros can
      expand into begin.
  **/
  public static Object evalToplevel(Object x, DynamicEnvironment env) {
    return currentEvaluator().evalToplevel(x, env);
  }

  //////////////// Evaluation ////////////////

  /** Evaluate an s-expression in the global environment. **/
  public static Object eval(Object x) {
    return currentEvaluator().eval(x);
  }

  /** Evaluate an s-expression in a lexical environment. First analyze
   * it.
   **/
  public static Object eval(Object x, Object env) {
    return currentEvaluator().eval(x, env);
  }

  /** Handle internal defines, and convert a list of exps to a single exp.
   * Examples: (x) => (begin x), (x y) => (begin x y),
   * ((define a 1) (+ a a)) => ((lambda (a) (begin (set! a 1) (+ a a))) #f)
   **/
  public static Object toBody(Object exps) {
    Pair parts = extractDefines(Pair.EMPTY, U.toPair(exps));
    Pair defines = ((Pair) parts.first);
    Pair body = ((Pair) parts.rest);
    if (U.isPair(defines)) {
      Pair vars = Pair.EMPTY;
      Pair sets = Pair.EMPTY;
      Pair vals = Pair.EMPTY;
      Pair ds = defines;
      while (U.isPair(ds)) {
	Pair d = ((Pair) ds.first);
	ds = ((Pair) ds.rest);
	vars = new Pair(U.second(d), vars);
	sets = new Pair(U.list(Symbol.SET, d.second(), d.third()), sets);
	vals = new Pair(U.FALSE, vals);
      }
      Pair begin = new Pair(Symbol.BEGIN, U.append(U.list(sets.reverse(),
                                                          body)));
      return new Pair(U.list(Symbol.LAMBDA, vars.reverse(), begin),
		      vals);
    } else return new Pair(Symbol.BEGIN, body);
  }

  /** Return a Pair who's first is a list of simple defines and who's
      rest is the remaining body.
  **/
  private static Pair extractDefines(Pair defines, Pair body) {
    if (!U.isPair(body)) return new Pair(defines.reverse(), body);
    else if (startsWith(body.first, Symbol.BEGIN))
      return extractDefines
        (defines, (Pair)U.append(U.list(U.rest(U.first(body)), body.rest)));
    else if (startsWith(body.first, Symbol.DEFINE))
      return extractDefines(new Pair(simplifyDefine(((Pair) body.first)),
                                     defines),
			    U.toList(body.rest));
    else return new Pair(defines.reverse(), checkForDefines(body));
    //    else return new Pair(defines.reverse(), body);
  }

  /** Check for embedded defines
   *   this checks for any improperly embedded defines
   *   and also flattens the body
   **/
  private static Object checkForDefines(Pair body) {
    if (!U.isPair(body)) return body;
    else if (startsWith(body.first, Symbol.BEGIN))
      return checkForDefines((Pair)U.append(U.list(U.rest(U.first(body)),
					    body.rest)));
    else if (startsWith(body.first, Symbol.DEFINE)) {
      return   E.error(
		       "Jscheme requires all embedded defines to appear first in procedure bodies\n"
		       +"You must move "+U.stringify(U.first(body))+" up\n");
    }
    else return new Pair(body.first,checkForDefines((Pair) body.rest));
  }

  /** Is the first element of the list identical to the given atom? **/
  private static boolean startsWith(Object list, Object atom) {
    return (U.isPair(list)) && (U.first(list) == atom);
  }

  private static Pair simplifyDefine(Pair definition) {
    Object var = U.second(definition);
    if (var instanceof Pair) {
      Pair var2 = (Pair) var;
      Object name = var2.first;
      Object args = var2.rest;
      Object body = U.rest(U.rest(definition));
      return
	new Pair(Symbol.DEFINE,
		 new Pair(name, U.list(new Pair(Symbol.LAMBDA,
						new Pair(args, body)))));
    } else return definition;
  }

}

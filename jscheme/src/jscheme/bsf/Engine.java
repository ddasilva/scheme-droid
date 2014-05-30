package jscheme.bsf;

import java.util.Vector;
import java.util.Enumeration;
import java.io.StringReader;
import jscheme.SchemeProcedure;
import jscheme.JScheme;
import jscheme.REPL;
import org.apache.bsf.BSFDeclaredBean;
import org.apache.bsf.BSFException;
import org.apache.bsf.BSFManager;
import org.apache.bsf.util.BSFEngineImpl;
import org.apache.bsf.util.BSFFunctions;
import org.apache.bsf.util.CodeBuffer;

/**
   Engine for running JScheme in the
   <a href="http://jakarta.apache.org/bsf/">Bean Scripting Framework</a>.

   <p>It does not implement the compile methods because i couldn't
   figure out what a CodeBuffer was for.
**/

public class Engine extends BSFEngineImpl {

  private JScheme js = new JScheme();

  /** Apply a procedure to arguments. **/
  public Object apply(String source,
		      int lineNo,
		      int columnNo,
		      Object funcBody,
		      Vector paramNames,
		      Vector arguments)
    throws BSFException {
    try {return js.apply((SchemeProcedure) funcBody,
	   (arguments.toArray(new Object[arguments.size()])));
    } catch (Throwable t) {return error(t);}
  }

  /** Assume object is null for now. **/
  public Object call (Object object, String name, Object[] args)
    throws BSFException {
    return (((SchemeProcedure)js.getGlobalValue(name)).apply(args));
  }

  /** A DeclaredBean can be referred to as a free variable. **/
  public void declareBean (BSFDeclaredBean bean) throws BSFException {
    js.setGlobalValue(bean.name, bean.bean);
  }

  public Object eval(String source, int lineNo, int columnNo, Object expr)
    throws BSFException {
    return js.eval(expr);
  }

  private Object error(Throwable e) throws BSFException {
    if (e == null) return null;
    else throw new BSFException(100, "Yow!", e);
  }

  public void exec(String source, int lineNo, int columnNo, Object script)
    throws BSFException {
    try {
      if (script instanceof String) {
	Enumeration es = REPL.readStream(new StringReader((String) script));
	while (es.hasMoreElements()) js.eval(es.nextElement());
      }
      else			// What should this do?
	js.call((SchemeProcedure) script);
    } catch (Throwable e) { error(e); }
  }

  public void initialize (BSFManager mgr,
			  String lang,
			  Vector declaredBeans) throws BSFException {
    super.initialize (mgr, lang, declaredBeans);
    // Reference the REPL class to get primitives loaded.
    Class repl = REPL.class;
    // register the mgr with object name "bsf"
    js.setGlobalValue ("bsf", new BSFFunctions (mgr, this));

    int size = declaredBeans.size ();
    for (int i = 0; i < size; i++)
      declareBean ((BSFDeclaredBean) declaredBeans.elementAt (i));
  }

  public void undeclareBean (BSFDeclaredBean bean) throws BSFException {
	js.setGlobalValue (bean.name, jsint.U.UNDEFINED);
  }
}

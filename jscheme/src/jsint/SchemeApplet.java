package jsint;
/**  
 * A jsint.SchemeApplet is an applet wrapper for a jsint.Scheme program
 * It reads the applet parameters to find which program to run
 * and then it creates a scheme interpreter to run that program.
 * @author Timothy J. Hickey, Copyright 2000, tim@cs.brandeis.edu, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */

import java.awt.*;
import java.net.URL;
import java.net.MalformedURLException;
import java.util.*;
import java.applet.*;

import jsint.*;

/**
 * this class defines an applet which will read
 * a file name from the applet parameter list and
 * will invoke the Jscheme interpreter on that file
 */
public class SchemeApplet extends java.applet.Applet
{
    Symbol init,start,stop,destroy;

   public void init()
   { 
      String 
	  progS    = this.getParameter("prog"),    // Scheme program to load
	  exprS    = this.getParameter("expr"),    // Scheme expression to evaluate
	  codeS    = this.getParameter("compiledprog"),    // compiled Scheme program to load
	  initS   = this.getParameter("init"),    // Scheme procedure to call when initializing
	  startS  = this.getParameter("start"),   // Scheme procedure to call when initializing
	  stopS   = this.getParameter("stop"),    // Scheme procedure to call when initializing
	  destroyS= this.getParameter("destroy"); // Scheme procedure to call when initializing

      if (initS != null) init = Symbol.intern(initS);
      if (startS != null) start = Symbol.intern(startS);
      if (stopS != null) stop = Symbol.intern(stopS);
      if (destroyS != null) destroy = Symbol.intern(destroyS);

      setBackground(new Color(250,150,255));
      setVisible(true);

      try {
	  if (codeS != null)
	      Scheme.eval(new Pair(Symbol.intern(codeS.concat(".load")), Pair.EMPTY));

          if (progS != null)
	      try {
		  Scheme.load(new InputPort(new URL(getDocumentBase(),progS).openStream()));
	      }catch(Exception e) {
		  Scheme.eval(U.list(Symbol.intern("load"),progS));}

	  if (exprS != null)
	      Scheme.eval((new InputPort((new java.io.StringReader(exprS)))).read());

          if (init != null)
             Scheme.eval(new Pair(init, new Pair(this, Pair.EMPTY)));

      } catch (Exception e) {E.warn("I/O Exception: "+ e); e.printStackTrace();}
   }
   
    public void start() {
        try {
            if (start != null)
                Scheme.eval(new Pair(start, new Pair(this, Pair.EMPTY)));
        } catch(Exception e){E.warn("I/O Exception: "+ e); e.printStackTrace();}
    }

    public void stop() {
        try {
            if (stop != null)
                Scheme.eval(new Pair(stop, new Pair(this, Pair.EMPTY)));
        } catch(Exception e){E.warn("I/O Exception: "+ e); e.printStackTrace();}
    }

    public void destroy() {
        try {
            if (destroy != null)
                Scheme.eval(new Pair(destroy, new Pair(this, Pair.EMPTY)));
        } catch(Exception e){E.warn("I/O Exception: "+ e); e.printStackTrace();}
    }

}




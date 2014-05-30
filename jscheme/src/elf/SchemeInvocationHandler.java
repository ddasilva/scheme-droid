package elf;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import jsint.Procedure;
import jscheme.JScheme;

/**
   <p>
   This InvocationHandler takes a Scheme procedure that is called on
   each invoke() with arguments proxy, method and args (the Object[]
   of arguments.

   <p> For examples of this, see the
   <a href="http://www.cs.rice.edu/~matthias/Scheme2000/anderson.ps">Scheme2000
   paper</a>, and
   http://developer.java.sun.com/developer/TechTips/2000/tt0530.html
 **/
public class SchemeInvocationHandler implements InvocationHandler {

  Procedure proc;

  public SchemeInvocationHandler(Procedure proc) { this.proc = proc; }

  public Object invoke(Object proxy, Method method, Object[] args)
    throws Throwable {
    return JScheme.forCurrentEvaluator().call(proc, proxy, method, args);
  }
}

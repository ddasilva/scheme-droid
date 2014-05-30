package jschemeweb;

/**
 * SchemeServlet
 *
 *   This helper class allows one to define an HttpServlet in which the
 *   public methods are implemented by scheme procedures.
 *   The servlet is initialized using a scheme procedure 
 *   specified in the web.xml file defining the servlet. 
 *
 * @author Timothy J. Hickey Copyright 2000, All Rights Reserved, <tim@cs.brandeis.edu>
 */

import java.io.*;
import java.text.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;

public class SchemeServlet extends HttpServlet {

    public static jscheme.JScheme js = new jscheme.JScheme();

  public jsint.Procedure
    do_service=null, 
    do_delete=null, 
    do_get=null, 
    do_options=null, 
    do_post=null,
    do_put=null,
    do_trace=null,
    do_destroy=null; 

  /**
     This reads a scheme initialization procedure from the resource
     specified by the init-param with name "code" in the web.xml The
     procedure is then evaluated and applied to this servlet.  The
     procedure can optionally set the do_??? variables of this servlet
     to scheme procedures which will override the defaults.
  */
  public void init() throws ServletException {
    ServletConfig config = this.getServletConfig();
    if (config == null) return;
    try {
      String file=config.getInitParameter("code");
      ServletContext sc = config.getServletContext();
      if (!(file == null)) {
         jsint.InputPort in = new jsint.InputPort (sc.getResourceAsStream(file));
         js.apply((jsint.Procedure) js.eval(in.read()),js.list(this));
         System.out.println("No code parameter for SchemeServlet\n ");
      }
    } catch (Exception e) {
      System.out.println("During SchemeServlet.init(): ");
      e.printStackTrace();
    }
  }

  public void doDelete(HttpServletRequest request,HttpServletResponse response)
    throws IOException, ServletException {
    if (do_delete != null)
       js.apply(do_delete, js.list(request, response));
    else super.doDelete(request, response);  }

  public void doGet(HttpServletRequest request, HttpServletResponse response)
    throws IOException, ServletException {
    if (do_get != null)
       js.apply(do_get, js.list(request, response));
    else super.doGet(request, response);  }

  public void doOptions(HttpServletRequest request, HttpServletResponse response)
    throws IOException, ServletException {
    if (do_options != null)
       js.apply(do_options, js.list(request, response));
    else super.doOptions(request, response);  }

  public void doPut(HttpServletRequest request, HttpServletResponse response)
    throws IOException, ServletException {
    if (do_put != null)  
       js.apply(do_put, js.list(request, response));
    else super.doPut(request, response);  }

  public void doTrace(HttpServletRequest request, HttpServletResponse response)
    throws IOException, ServletException {
    if (do_trace != null)
       js.apply(do_trace, js.list(request, response));
    else super.doTrace(request, response);  }

  public void doPost(HttpServletRequest request, HttpServletResponse response)
    throws IOException, ServletException {
    if (do_post != null)
       js.apply(do_post, js.list(request, response));

    else super.doPost(request, response);   }

  public void service(HttpServletRequest request,  HttpServletResponse response)
    throws IOException, ServletException {
    if (do_service != null)
       js.apply(do_service, js.list(request, response));
    else super.service(request, response); }

  public void destroy() {
    if (do_destroy != null)
       js.apply(do_destroy,js.list());
    else super.destroy();
  }

}

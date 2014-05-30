package interact;

import java.awt.Container;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.PrintWriter;
import java.io.Reader;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import jscheme.JScheme;
import jsint.InputPort;
import jsint.Procedure;
import jsint.Scheme;
import jsint.U;

/**
   An Interactor provides a window for interacting with a Jscheme
   listener that is useful for debugging Java applications.  A user
   can provide an Interactor with name value pairs that are bound as
   Jscheme global variables for easy reference.

   <p>Currently there can be only one Interactor at a time.  If a
   second one is started, because of global io variables, The old
   window will become useless.
 **/
public class Interactor implements Runnable {
  
  JFrame frame;
  private IOTextArea area;
  private IOTextArea getArea() {
    if (this.area == null) {
      this.area = new IOTextArea(rows, cols);
      this.area.setFont(new Font("monospaced", Font.PLAIN, 12)); }
    return this.area; }
      
  private Reader in;
  private Reader getReader() { 
    if (in == null) this.in =  this.getArea().getReader();
    return this.in; }
  
  private PrintWriter out;
  private PrintWriter getWriter() {
    if (out == null) out = new PrintWriter(getArea().getWriter()); 
    return this.out; }

  String name;
  int rows;
  int cols;
  Object[] pairs;
  String[] files;
  JScheme js;
  
  /**
     Start a new interactor in a new thread with Object, it, bound
     to the Jscheme global <tt>it</tt>.
   **/
  public Interactor(Object it) {this(it, new JScheme());}

  /**
     Start a new interactor in a new thread with Object, it, bound
     to the Jscheme global <tt>it</tt>.
   **/
  public Interactor(Object it, JScheme js) {this(new Object[] { "it", it }, js);}

  /**
     Start a new interactor in a new thread with the Object[], Pairs,
     providing String "name", Object value pairs that
     become Jscheme global bindings.
   **/
  public Interactor(Object[] pairs, JScheme js) {
    this(true, "Interactor", pairs, 24, 80, null, js);
  }

  /** Most general Interactor constructor.
      @param newThread Start Interactor in a new thread?
      @param name Name of JFrame.
      @param pairs Name value pairs bound to Jscheme global variables.
      @param rows Number of rows.
      @param cols Number of columns.
      @param files Additional arguments as in jsint.Scheme.main().
   **/
  public Interactor(final boolean newThread, 
		    final String name, 
		    final Object[] pairs, 
		    final int rows, 
		    final int cols, 
		    final String[] files, 
                    final JScheme js) {
    this.name  = name;
    this.pairs = pairs;
    this.rows  = rows;
    this.cols  = cols;
    this.files = files;
    this.frame = createFrame();
    this.js = js;
    if (newThread) new Thread(this).start();
    else this.run();
  }
  
  private JFrame createFrame() {
    JFrame frame = new JFrame(name);
    frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	// KRA 11OCT99: It would be nice if we could stop Scheme too.
	// But the user must do it.
	// e.getWindow().dispose(); 
	Toolkit.getDefaultToolkit().beep();
	Interactor.this.getWriter().println("Type (exit) to exit this Frame!");
      }});
    Container pane = frame.getContentPane();
    JScrollPane scroll = new JScrollPane(this.getArea()); 
    pane.add(scroll, "Center");
    frame.pack();
    frame.setVisible(true);
    return frame;
  }

  public void readEvalWriteLoop(String prompt) {
    Scheme.readEvalWriteLoop(prompt);
  }

  public void run() {
    // These should be dynamic variables!
    Scheme.pushEvaluator((jsint.Evaluator) js.getEvaluator());
    jsint.BacktraceException.printJavaTrace = true;
    jsint.Evaluator e = Scheme.currentEvaluator();
    e.setInput(new InputPort(this.getReader()));
    e.setOutput(this.getWriter());
    e.setError(this.getWriter());
    js.evalOrLoad("elf/basic.scm"); // Load some useful behavior.
    this.importVariables();
    
    if(files != null) this.loadFiles(files);
    readEvalWriteLoop("> ");
    this.frame.dispose();
    this.frame = null;
    Scheme.popEvaluator();
  }

  private void loadFiles(String[] args) {
    for(int i = 0; i < ((args == null) ? 0 : args.length); i++)
      js.evalOrLoad(args[i]);
  }

  /** Import variable value pairs.  Import the class of the value just
      to be safe.  Show the bindings so the user can use them. **/
  private void importVariables() {
    Procedure importer = ((Procedure) js.getGlobalValue("import"));
    if (pairs != null) {
      js.call("display", "Bindings:\n");
      for (int i = 0; i < pairs.length; i = i + 2)
	importVariable(((String) pairs[i]), pairs[i+1]);
    }
  }

  private void importVariable(String var, Object val) {
    js.setGlobalValue(var, val);
    js.call("import", val.getClass().getName());
    js.call("display",var + " = " + U.stringify(val) + "\n",
            Scheme.currentEvaluator().getOutput());
  }

  /**
     Run the interactor.

   **/
  public static void main(String[] args) {
    new Interactor(false, "Interactor",
		   new Object[] { "now", new java.util.Date() },
		   24, 80, args, new JScheme());
    System.out.println("Interaction done!");
    System.exit(0);
  }
}

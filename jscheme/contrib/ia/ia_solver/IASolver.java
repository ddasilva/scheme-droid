package ia_solver;

public class IASolver {
   private static jscheme.JScheme js = new jscheme.JScheme();
    static {js.eval("(load \"ia_solver/ia-solver.scm\")");}

    /**
     *  This attempts to solve the constraint represented by the string expr
     *  by creating a contractor and applying in N times. The hashtable H
     *  provides an initial assignment of variables to RealInterval's
     *  If the constraint is proved to be unsatisifable, then it returns false
     *  and returns true otherwise.
    **/
    public static boolean solve(String expr, int N, java.util.Hashtable H) {
        Object result = js.call("solve", expr, new Integer(N), H);
        return  (result instanceof java.util.Hashtable);
    }
   
    /**
     * This attempts to solve the constraint in the string expr as above,
     * but if unsuccessful it returns null and if successful it returns
     * a hashtable containing the intervals in which each variable must lie.
     **/
    public static java.util.Hashtable solve(String expr, int N) {
        Object result = js.call("solve", expr, new Integer(N), new java.util.Hashtable());
        if (result instanceof java.util.Hashtable)
	    return (java.util.Hashtable)  result;
	else return null;
    }
   
    public static void main(String[] args) {
        if (args.length != 2)
	    {
             System.out.println(
                "\n  usage: java ia_solver.IASolver NUM \"CONSTRAINTS SEPARATED BY SEMICOLONS\"\n");
             System.out.println(
                "\n  e.g.: java ia_solver.IASolver 100 \"x^2+y^2+z^2=25; y=exp(x);z=cos(z);x>0\"\n");
	    return;}

        java.util.Hashtable h = new java.util.Hashtable();
        System.out.println("Solving:\n " + args[1] + "\n using "+ args[0] + " constraint contractions\n\n");
	if (solve(args[1],(new Integer(args[0])).intValue(),h))
	    System.out.println("The solutions (if any) must lie in the following intervals\n "+h+"\n");
        else System.out.println("The system has no solutions\n");
    }
}

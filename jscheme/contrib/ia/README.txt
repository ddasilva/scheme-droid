ia_solver -- a JScheme/Java solver for non-linear constraints.

Use:

To use the solver to solve a constraint you need to ...

1) download the ia_solver package and cd into

2) give the following commmand
   java -cp lib/jscheme.jar:lib/ia_math.jar:. ia_solver.IASolver 100 "x^2+y^2=25; y=exp(x); x>0"

It will return a hashtable of the bindings for x and y
(or will state that the system is unsatisfiable)



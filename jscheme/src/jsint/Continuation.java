package jsint;

/**
   A continuation.

   @author Peter Norvig, Copyright 1998, peter@norvig.com, <a
   href="license.txt">license</a> subsequently modified by Jscheme
   project members licensed under zlib licence (see license.txt)
  */

public class Continuation extends Procedure {

    RuntimeException cc;
    public Object value;

    public Continuation(RuntimeException cc) { 
        super(1,1); 
        this.cc = cc; }

    public Object apply(Object[] args) {
        value = args[0];
        throw cc;
    }
}



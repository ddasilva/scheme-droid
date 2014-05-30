package jsint;

/**  
 * A jsint.JschemeThrowable is used to support throwing and catching
 * of objects in Jscheme programs.
 * @author Timothy J. Hickey, Copyright 2000, tim@cs.brandeis.edu, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */

public class JschemeThrowable extends RuntimeException {
    public Object contents;

    public JschemeThrowable(Object contents) {
	this.contents = contents;
    }

    public JschemeThrowable(String message, Object contents) {
        super(message);
	this.contents = contents;
    }

    public String toString(){
	return ("JschemeThrowable:[["+ U.stringify(this.getMessage(),false)+
                ","+ U.stringify(contents,false))+"]]";
    }

  /** Clever performance trick i found in
      http://docs.msdnaa.net/ark_new/Webfiles/WhitePapers/Babel01/bab12.pdf
  **/
  public Throwable fillInStackTrace() {
    return this;
  }
}

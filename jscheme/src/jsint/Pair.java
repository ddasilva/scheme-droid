package jsint;

/** A Pair has two fields, first and rest (sometimes called car and cdr). 
 * The empty list is represented as a Pair, with both fields empty.
 * (In Scheme it is an error to ask access those fields, but we don't complain.) 
 * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
**/

public class Pair implements java.io.Serializable, jscheme.SchemePair {

  /** The first element of the pair. **/
  public Object first;

  /** The other element of the pair. **/
  public Object rest;

  /** The empty list. It's first and rest fields are also EMPTY. **/
  public static final Pair EMPTY = new Pair(null, null);
  
  static {EMPTY.first = EMPTY.rest = EMPTY;}
  
  /** Build a pair from two components. **/
  public Pair(Object first, Object rest) { 
      this.first = first; this.rest = rest; 
  }

  /** Return the first element of a list. **/
  public Object getFirst() {
      return first;
  }

  /** Return the second element of a list. **/
  public Object getRest() {
      return rest;
  }


  /** Set the first element of a list. **/
  public Object setFirst(Object x) {
      Object y = first;
      this.first=x;
      return y;
  }

  /** Return the second element of a list. **/
  public Object setRest(Object x) {
      Object y = rest;
      this.rest=x;
      return y;
  }


  /** Return the second element of a list. **/
  public Object first() {
      return first;
  }

  /** Return the second element of a list. **/
  public Object rest() {
      return rest;
  }



  /** Return the second element of a list. **/
  public Object second() {
    return U.toPair(rest).first;
  }

  /** Return the third element of a list. **/
  public Object third() {
    return U.second(rest);
  }

  /** Reverse the elements of a list. **/
  public Object reverse() {
    Object result = Pair.EMPTY;
    Pair list = this;
    while (list != EMPTY) {
      if (Scheme.isInterruptable()) Scheme.interruptCheck();
      result = new Pair(list.first, result);
      list = U.toList(list.rest);
    }
    return result;
  }
  
  /** Lists that are .equals have the same .hashCode(). **/
  public int hashCode() {
    if (this == EMPTY) return super.hashCode();
    else {
      int head = hashCode0(this.first);
      int tail = hashCode0(this.rest);
    return head + tail*37;
    }
  }

  private static int hashCode0(Object it) {
    if (it == null) return 17;
    else if (it instanceof Object[]) return arrayHashCode((Object[]) it);
    else return it.hashCode();
  }

  private static int arrayHashCode(Object[] xs) {
    int code = 0;
    for (int i = 0; i < xs.length; i++)
      code = code + xs[i].hashCode()*37;
    return code;
  }

  /** Two pairs are equal if their first and rest fields are (equal?). **/
  public boolean equals(Object that) {
    if (this == that) return true;
    else return (that instanceof Pair) && (that != EMPTY) && (this != EMPTY)
	   && this.equalsFirst((Pair)that)
	   && U.equal(this.rest, ((Pair) that).rest);
  }

  private boolean equalsFirst(Pair that) {
    return  (this.first == null) ? that.first == null :
      U.equal(this.first, ((Pair) that).first);
  }

  /** Return a String representation of the pair. **/
  public String toString() {
    if (this == EMPTY) return "()";
    else return this.stringifyPair(true, new StringBuffer()).toString(); 
  }

  /** Build up a String representation of the Pair in a StringBuffer. **/
  public StringBuffer stringifyPair(boolean quoted, StringBuffer buf) {
    String special = null;
    if ((U.isPair(rest)) && U.rest(rest) == EMPTY)
      special = (first == Symbol.QUOTE) ? "'" 
	: (first == Symbol.QUASIQUOTE) ? "`"
        : (first == Symbol.UNQUOTE) ? "," 
	: (first == Symbol.UNQUOTE_SPLICING) ? ",@"
        : null;
        
    if (special != null) {
      buf.append(special); U.stringify(U.second(this), quoted, buf);
    } else {
      buf.append('(');
      U.stringify(first, quoted, buf);
      Object tail = rest;
      while (U.isPair(tail)) {
	if (Scheme.isInterruptable()) Scheme.interruptCheck();
        buf.append(' ');
        U.stringify(((Pair)tail).first, quoted, buf);
        tail = ((Pair)tail).rest;
      }
      if (tail != EMPTY) {
        buf.append(" . ");
        U.stringify(tail, quoted, buf);
      }
      buf.append(')');
    }
    return buf;
  }
  
  /** The length of a proper list.  **/
  public int length() {
    int len = 0;
    Object list = this;
    while (U.isPair(list)) {
      if (Scheme.isInterruptable()) Scheme.interruptCheck();
      len++;
      list = U.rest(list);
    }
    return len;
  }

  /** Find the nth element of a list, zero-based **/
  public Object nth(int n) { return U.toPair(listTail(n)).first; }

  /** Find the nth tail of a list, zero-based. **/
  public Object listTail(int n) {
    int orig = n;
    Pair list = this;
    while (n > 0) { 
        n--; 
        if (list.isEmpty())
	    E.error("attempting to compute listTail "+orig+" which is greater than the length of the list",this);
        else
	    list = U.toList(list.rest); }
    return list;
  }                

  public boolean isEmpty() { return this == Pair.EMPTY; }

    // COMPILER SUPPORT METHODS

    //    // get an element or a tail of a list using "n/2" notation:
    //    public Object getEltNover2(int n) {
    //	if ((n % 2)== 0) 
    //	    return (this.listTail(n/2));
    //	return (this.nth(n/2));
    //    }
    //
    //    // set an element of a list (using "n/2" notation)
    //    public Object setEltNover2(int n,Object v) {
    //        Object oldv;
    //	if ((n % 2)== 0) return E.error("Error in Compiler: setEltOver2 "+n);
    //        oldv = this.nth(n/2);
    //        ((Pair) this.listTail(n/2)).first = v;
    //	return (oldv);
    //    }

    public Object getEltNover2(int n) {
     Pair t = this;
	while ((n>1) && (t.rest != null)) {
            t=((Pair) t.rest);
            n -= 2;}
        if (n==0)
          return t;
        if (n==1)
          return (t.first);
        else return null;
    }

    public Object setEltNover2(int n, Object v) {
     Pair t = this;
	while ((n>1) && (t.rest != null)) {
            t=((Pair) t.rest);
            n -= 2;}
        if (n==1)
            return (t.first=v);
        else
 	    {E.warn("ERROR in setEltNover2"); return null;}
    }

  private Object readResolve() throws java.io.ObjectStreamException {
    return this.first == this && this.rest == this ? EMPTY : this;
  }
}

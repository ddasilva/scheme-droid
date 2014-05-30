package jscheme;

/** A SchemePair has two fields, first and rest (sometimes called car and cdr). 
 * SchemePairs are constructed using the factory method REPL.makeSchemePair(first,rest).
 * There is a distinguished SchemePair REPL.EMPTY_PAIR, whose first and rest are itself.
 * It represents the empty list.
 * @author Tim Hickey, Copyright 2001, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
**/

public interface SchemePair {
  public Object getFirst();
  public Object getRest();
  public Object setFirst(Object x);
  public Object setRest(Object x);

  public Object first();
  public Object rest();
  public Object second(); 
  public Object third();
  public Object reverse();
  public int hashCode();
  public boolean equals(Object that);
  public String toString();
  public StringBuffer stringifyPair(boolean quoted, StringBuffer buf);
  public int length();
  public Object nth(int n);
  public Object listTail(int n);
  public boolean isEmpty();
  public Object getEltNover2(int n);
  public Object setEltNover2(int n, Object v);
}

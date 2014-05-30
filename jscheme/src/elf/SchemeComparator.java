package elf;

/**
   A Comparator, for activities like sorting, that lets a procedure do
   the work of comparing.
 **/

public class SchemeComparator implements java.util.Comparator {
  jsint.Procedure proc;

  public SchemeComparator(jsint.Procedure proc) { this.proc = proc; }

  public int compare(Object a, Object b) {
    return jscheme.JScheme.intValue(jscheme.JScheme.forCurrentEvaluator().call(proc, a, b)); }

  public boolean equals(Object that) {
    return this == that ||
      this.getClass() == that.getClass() &&
      this.proc == ((SchemeComparator) that).proc; }
      
  public int hashCode() { return proc.hashCode(); }
}

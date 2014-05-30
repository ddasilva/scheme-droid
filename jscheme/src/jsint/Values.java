package jsint;
/** Values provided by Derek Upham <Derek.Upham@ontain.com> **/

public class Values implements java.io.Serializable {
  public static Object values(Pair args) {
    int len = args.length();
    // if (args.length() == 1) return args.first;
    if (!args.isEmpty() && ((Pair) args.rest).isEmpty()) return args.first;
    else return new Values(args);
  }

  public static Object callWithValues(Procedure source, Procedure sink) {
    Object result = source.apply(Pair.EMPTY);
    if (result instanceof Values)
      return sink.apply(((Values) result).rep);
    else return sink.apply(new Pair(result, Pair.EMPTY));
  }

  private Pair rep;

  public Values(Pair list) { rep = list; }

  public String toString() {
    String newline = System.getProperty("line.separator");
    StringBuffer sb = new StringBuffer();
    Pair p = rep;
    if (!p.isEmpty()) sb.append(U.stringify(p.first));
    for(p=((Pair) p.rest); !p.isEmpty(); p=((Pair) p.rest)) {
      sb.append(newline);
      sb.append(U.stringify(p.first));
    }
    return sb.toString();
  }
}

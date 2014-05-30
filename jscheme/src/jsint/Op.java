package jsint;

/**
 * This class provides methods for those scalar operations which cannot
 * be obtained using reflection on the standard Java libraries.
 * @author Timothy J. Hickey, Copyright 2000, tim@cs.brandeis.edu, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */

public class Op {

  public static final int 
    ADD=0,   // +
    SUB=1,   // -
    MUL=2,   // *
    DIV=3,   // /
    MOD=4,   // %
    AND=5,   // A&B
    OR= 6,   // A|B 
    XOR=7,   // A^B
    IMP=8,   // (-A)|B, so A IMP #F == ~A
    LSH=9,   // <<
    RSH=10,  // >>
    RSHZ=11, // >>>
    EQ=12,   // =
    LT=13,   // <
    GT=14,   // >
    LE=15,   // <=
    GE=16,   // >=
    NE=17,   // !=
    COMPLEMENT=18, // ~
    NEGATE=19, // -
    SGN=20, //
    MODULO=21   // R4RS modulo
    ;

  public static Number add(Object a, Object b) {return genericBinaryOp(Op.ADD,a,b);}
  public static Number sub(Object a, Object b) {return genericBinaryOp(Op.SUB,a,b);}
  public static Number mul(Object a, Object b) {return genericBinaryOp(Op.MUL,a,b);}
  public static Number div(Object a, Object b) {return genericBinaryOp(Op.DIV,a,b);}
  public static Number mod(Object a, Object b) {return genericBinaryOp(Op.MOD,a,b);}
  public static Number modulo(Object a, Object b) {return genericBinaryOp(Op.MODULO,a,b);}
  public static Number negate(Object a) {return genericUnaryOp(Op.NEGATE,a);}
  public static Number complement(Object a) {return genericUnaryOp(Op.COMPLEMENT,a);}
  public static Number sgn(Object a) {return genericUnaryOp(Op.SGN,a);}

  public static Number leftShift(Object a, Object b) {return genericBinaryOp(Op.LSH,a,b);}
  public static Number rightShift(Object a, Object b) {return genericBinaryOp(Op.RSH,a,b);}
  public static Number rightShiftZ(Object a, Object b) {return genericBinaryOp(Op.RSHZ,a,b);}

  public static boolean eq(Object a, Object b) {return genericBinaryComp(Op.EQ,a,b);}
  public static boolean lt(Object a, Object b) {return genericBinaryComp(Op.LT,a,b);}
  public static boolean gt(Object a, Object b) {return genericBinaryComp(Op.GT,a,b);}
  public static boolean le(Object a, Object b) {return genericBinaryComp(Op.LE,a,b);}
  public static boolean ge(Object a, Object b) {return genericBinaryComp(Op.GE,a,b);}
  public static boolean ne(Object a, Object b) {return genericBinaryComp(Op.NE,a,b);}
     
  public static Number and(Object a, Object b) {return genericBinaryOp(Op.AND,a,b);}
  public static Number xor(Object a, Object b) {return genericBinaryOp(Op.XOR,a,b);}
  public static Number  or(Object a, Object b) {return genericBinaryOp(Op.OR ,a,b);}
  public static Number imp(Object a, Object b) {return genericBinaryOp(Op.IMP,a,b);}


  public static Number genericBinaryMultiOp(int op, Number acc, Pair args) {
    while (args != Pair.EMPTY) {
      acc = genericBinaryOp(op,acc,args.first);
      args = (Pair)(args.rest);
    }
    return acc;
  }


  private static Number genericBinaryOp(int op, Object a, Object b) {
    Class c = lubNumericClass(a.getClass(),b.getClass());
    if (c == Integer.class) 
      return binaryOpInteger( op, (Integer) coerceNumber(a,Integer.class), (Integer) coerceNumber(b,Integer.class));
    else if (c == Long.class   ) 
      return binaryOpLong( op, (Long)    coerceNumber(a,Long.class),    (Long)    coerceNumber(b,Long.class));
    else if (c == Float.class  ) 
      return binaryOpFloat( op, (Float)   coerceNumber(a,Float.class),   (Float)   coerceNumber(b,Float.class));
    else if (c == Double.class ) 
      return binaryOpDouble( op, (Double)  coerceNumber(a,Double.class),  (Double)  coerceNumber(b,Double.class));
    else return wrongTypeError(a, b);
  }

  private static boolean genericBinaryComp(int op, Object a, Object b) {
    Class c = lubNumericClass(a.getClass(),b.getClass());
    if (c == Integer.class) 
      return binaryCompInteger( op, (Integer) coerceNumber(a,Integer.class), (Integer) coerceNumber(b,Integer.class));
    else if (c == Long.class   ) 
      return binaryCompLong( op, (Long)    coerceNumber(a,Long.class),    (Long)    coerceNumber(b,Long.class));
    else if (c == Float.class  ) 
      return binaryCompFloat( op, (Float)   coerceNumber(a,Float.class),   (Float)   coerceNumber(b,Float.class));
    else if (c == Double.class ) 
      return binaryCompDouble( op, (Double)  coerceNumber(a,Double.class),  (Double)  coerceNumber(b,Double.class));
    else { wrongTypeError(a, b); return false; }
  }

  private static Number genericUnaryOp(int op, Object a) {
    Class c = lubNumericClass(a.getClass(),Integer.class);
    if (c == Integer.class) 
      return unaryOpInteger( op, (Integer) coerceNumber(a,Integer.class));
    else if (c == Long.class   ) 
      return unaryOpLong( op, (Long)    coerceNumber(a,Long.class));
    else if (c == Float.class  ) 
      return unaryOpFloat( op, (Float)   coerceNumber(a,Float.class));
    else if (c == Double.class ) 
      return unaryOpDouble( op, (Double)  coerceNumber(a,Double.class));
    else return wrongTypeError(a);
  }

  /**
     KRA 29SEP01: Previous version always constructed a new number.
     Evaluating (time (+ 1 1) 1000) consed 184 bytes per addition.
     <p> Now we get:
     <pre>
     (+ 1 1) 120
     (+ 1.0 1.0) 136
     (+ 1.0 1) 152
*/
  private static Number coerceNumber(Object a, Class c) {
    if (a instanceof Number) {
      if (a.getClass() == c) return (Number) a;
      else if (c==Integer.class) return U.toNum(((Number)a).intValue());
      else if (c==Double.class)  return U.toNum(((Number)a).doubleValue());
      // else if (c==Long.class) return U.toNum(((Number)a).longValue());
      else if (c==Long.class)    return new Long(((Number)a).longValue());
      else if (c==Float.class)   return new Float(((Number)a).floatValue());
      else return ((Number) E.error("Bad coersion "+a+" to "+ c));
    }
    if (a instanceof Character) return coerceNumber(new Integer((int)((Character)a).charValue()), c);
      else return ((Number) E.error("Bad coersion "+a+" to "+ c));
  }

  public static Character numberToChar(Number a) {
    return new Character((char) a.shortValue());
  }

  public static Number charToNumber(Character a) {
    return new Integer((int) a.charValue());
  }

  public static Class lubNumericClass(Class a, Class b) {
    if ((a==Byte.class) || (a==Short.class) || (a==Character.class)) 
      a = Integer.class;
    if ((b==Byte.class) || (b==Short.class) || (b == Character.class)) 
      b = Integer.class;
    if (a == b) return b;	// KRA 30DEC03: 7% improvement in (+ 1 1).
    if (a==Integer.class){
      if (b==Integer.class) return Integer.class;
      else if (b==Long.class) return Long.class;
      else if (b==Float.class) return Float.class;
      else return Double.class;}
    else if (a==Long.class) {
      if (b==Integer.class) return Long.class;
      else if (b==Long.class) return Long.class;
      else if (b==Float.class) return Float.class;
      else return Double.class;}
    else if (a==Float.class) {
      if (b==Integer.class) return Float.class;
      else if (b==Long.class) return Float.class;
      else if (b==Float.class) return Float.class;
      else return Double.class;}
    else 
      return Double.class;
  }

  private static Integer unaryOpInteger(int op, Integer a) {
    int a1=a.intValue(),
      c = 0;
    switch (op) {
    case COMPLEMENT: c= ~a1; break;
    case NEGATE:     c= -a1; break;
    case SGN:        if (a1 > 0) c=1; else c=-1; break;
    default: unknownOpError("unaryOp", op); break;
    }
    return new Integer(c);
  }

  private static Long unaryOpLong(int op, Long a) {
    long a1=a.longValue(),
      c = 0;
    switch (op) {
    case COMPLEMENT: c= ~a1; break;
    case NEGATE:     c= -a1; break;
    case SGN:        if (a1 > 0) c=1; else c=-1; break;
    default: unknownOpError("unaryOp", op); break;
    }
    return new Long(c);
  }

  private static Float unaryOpFloat(int op, Float a) {
    float a1=a.floatValue(),
      c = 0.0F;
    switch (op) {
    case NEGATE:     c= -a1; break;
    case SGN:        if (a1 > 0) c=1; else c=-1; break;
    default: unknownOpError("unaryOp", op); break;
    }
    return new Float(c);
  }

  private static Double unaryOpDouble(int op, Double a) {
    double a1=a.doubleValue(),
      c = 0.0;
    switch (op) {
    case NEGATE:     c= -a1; break;
    case SGN:        if (a1 > 0) c=1; else c=-1; break;
    default: unknownOpError("unaryOp", op); break;
    }
    return new Double(c);
  }

  private static Integer binaryOpInteger(int op, Integer a, Integer b) {
    int a1=a.intValue(),
      b1=b.intValue(),
      c = 0;
    switch (op) {
    case ADD: c=a1+b1; break;
    case SUB: c=a1-b1; break;
    case MUL: c=a1*b1; break;
    case DIV: c=a1/b1; break;
    case MOD: c=a1%b1; break;
    case MODULO: c=a1%b1; c= (((c==0)||((c>0)==(b1>0)))?c:c+b1); break;
    case AND: c=a1&b1; break;
    case XOR: c=a1^b1; break;
    case  OR: c=a1|b1; break;
    case IMP: c=(~a1)|b1; break;
    case LSH: c=(a1<<b1); break;
    case RSH: c=(a1>>b1); break;
    case RSHZ: c=(a1>>>b1); break;
    default: unknownOpError("binaryOp", op); break;
    }
    return U.toNum(c);
  }

  private static Long binaryOpLong(int op, Long a, Long b) {
    long a1=a.longValue(),
      b1=b.longValue(),
      c = 0L;
    switch (op) {
    case ADD: c=a1+b1; break;
    case SUB: c=a1-b1; break;
    case MUL: c=a1*b1; break;
    case DIV: c=a1/b1; break;
    case MOD: c=a1%b1; break;
    case MODULO: c=a1%b1; c=(((c==0)||((c>0)==(b1>0)))?c:(c+b1)); break;
    case AND: c=a1&b1; break;
    case XOR: c=a1^b1; break;
    case  OR: c=a1|b1; break;
    case IMP: c=(~a1)|b1; break;
    case LSH: c=(a1<<b1); break;
    case RSH: c=(a1>>b1); break;
    case RSHZ: c=(a1>>>b1); break;
    default: unknownOpError("binaryOp", op); break;
    }
    return new Long(c);
  }

  private static Float binaryOpFloat(int op, Float a, Float b) {
    float a1=a.floatValue(),
      b1=b.floatValue(),
      c = 0.0F;
    switch (op) {
    case ADD: c=a1+b1; break;
    case SUB: c=a1-b1; break;
    case MUL: c=a1*b1; break;
    case DIV: c=a1/b1; break;
    case MOD: c=a1 - b1*Math.round(a1/b1); break;

    default: unknownOpError("binaryOp", op); break;
    }
    return new Float(c);
  }

  private static Double binaryOpDouble(int op, Double a, Double b) {
    double a1=a.doubleValue(),
      b1=b.doubleValue(),
      c = 0.0;
    switch (op) {
    case ADD: c=a1+b1; break;
    case SUB: c=a1-b1; break;
    case MUL: c=a1*b1; break;
    case DIV: c=a1/b1; break;
    case MOD: c=a1 - b1*Math.round(a1/b1); break;
    default: unknownOpError("binaryOp", op); break;
    }
    return new Double(c);
  }

  // binary comparisons
  private static boolean binaryCompInteger(int op, Integer a, Integer b) {
    int a1=a.intValue(),
      b1=b.intValue();
    boolean c = false;
    switch (op) {
    case EQ: c=(a1==b1); break;
    case LT: c=(a1<b1); break;
    case GT: c=(a1>b1); break;
    case LE: c=(a1<=b1); break;
    case GE: c=(a1>=b1); break;
    case NE: c=(a1!=b1); break;
    default: unknownOpError("binaryComp", op); break;
    }
    return c;
  }


  private static boolean binaryCompLong(int op, Long a, Long b) {
    long a1=a.longValue(),
      b1=b.longValue();
    boolean c = false;
    switch (op) {
    case EQ: c=(a1==b1); break;
    case LT: c=(a1<b1); break;
    case GT: c=(a1>b1); break;
    case LE: c=(a1<=b1); break;
    case GE: c=(a1>=b1); break;
    case NE: c=(a1!=b1); break;
    default: unknownOpError("binaryComp", op); break;
    }
    return c;
  }


  private static boolean binaryCompFloat(int op, Float a, Float b) {
    float a1=a.floatValue(),
      b1=b.floatValue();
    boolean c = false;
    switch (op) {
    case EQ: c=(a1==b1); break;
    case LT: c=(a1<b1); break;
    case GT: c=(a1>b1); break;
    case LE: c=(a1<=b1); break;
    case GE: c=(a1>=b1); break;
    case NE: c=(a1!=b1); break;
    default: unknownOpError("binaryComp", op); break;
    }
    return c;
  }


  private static boolean binaryCompDouble(int op, Double a, Double b) {
    double a1=a.doubleValue(),
      b1=b.doubleValue();
    boolean c = false;
    switch (op) {
    case EQ: c=(a1==b1); break;
    case LT: c=(a1<b1); break;
    case GT: c=(a1>b1); break;
    case LE: c=(a1<=b1); break;
    case GE: c=(a1>=b1); break;
    case NE: c=(a1!=b1); break;
    default: unknownOpError("binaryComp", op); break;
    }
    return c;
  }

  public static boolean eqv(Object a, Object b) {
    if ((a instanceof Number) && (b instanceof Number)) return(eq((Number)a,(Number)b));
    else if ((a==null) || (b==null)) return (a==b);
    else return (a.equals(b));
  }

  public static boolean sameObject(Object a, Object b) {
    return (a == b);
  }

  private static Number wrongTypeError(Object a, Object b) {
    return ((Number) E.error("binaryOp -- wrong types" +
			     a.getClass() + " " + b.getClass()));
  }

  private static Number wrongTypeError(Object a) {
    return ((Number) E.error("unaryOp -- wrong type" + a.getClass()));
  }

  private static Object unknownOpError(String name, int op) {
    return E.error("Error in " + name +
		   " -- unknown operator number: " + op);
  }

  public static Number addMulti(Pair x) {
    return U.isPair(x) ? genericBinaryMultiOp
      (Op.ADD, toNumber(x.first), (Pair) x.rest) : U.toNum(0);
  }

  public static Number mulMulti(Pair x) {
    return U.isPair(x) ? genericBinaryMultiOp
      (Op.MUL, toNumber(x.first), (Pair) x.rest) : U.toNum(1);
  }
  
  public static Number toNumber(Object x) {
    return x instanceof Number ? (Number) x :
      x instanceof Character ? new Integer((int)((Character)x).charValue()) :
      ((Number) E.error (x + " can't be converted to a Number"));
  }
}

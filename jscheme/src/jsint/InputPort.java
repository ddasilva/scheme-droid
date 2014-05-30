
package jsint;
import java.io.*;

/** InputPort is to Scheme as InputStream is to Java. 

 * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
**/

public class InputPort implements java.util.Enumeration {

  /** The distinguished end of file marking object. **/
  // public static final Symbol EOF = Symbol.intern("#!eof");
  public static final Object EOF = token("!eof");
  public static final boolean defaultBrlsMode=true;
  public boolean brlsMode=defaultBrlsMode;
  private boolean keepComments = false;
    
  /** These tokens must be distinguishable from anything else returned
      by readToken().
  **/
  private static class Token {
    String name;

    Token(String name) { this.name = name; }
    public String toString() { return "#" + name + "#"; }
  }

  private static final Object BACKQUOTE = token("`");
  private static final Object CLOSE = token(")");
  private static final Object COMMA = token(",");
  private static final Object COMMA_AT = token(",@");
  private static final Object DOT = token(".");
  private static final Object OPEN = token("(");
  private static final Object QUOTE = token("'");
  private static final Object HASH_ESCAPE_CLOSE = token("#]");
  private static final Object HASH_ESCAPE_OPEN = token("#[");
  private static final Object HASH_CLOSE = token("#}");

  /* These are used when reading the #\s,#\space,#\n,#\newline
     character literals */
  private static final Symbol CHAR_s = Symbol.intern("s");
  private static final Symbol CHAR_S = Symbol.intern("S");
  private static final Symbol CHAR_n = Symbol.intern("n");
  private static final Symbol CHAR_N = Symbol.intern("N");
  private static final Symbol NEWLINE_UC = Symbol.intern("NEWLINE");
  private static final Symbol NEWLINE_LC = Symbol.intern("newline");
  private static final Symbol SPACE_UC = Symbol.intern("SPACE");
  private static final Symbol SPACE_LC = Symbol.intern("space");
  private static final Symbol CURLY = Symbol.intern("!{}");
  private static final Symbol HASH_CURLY = Symbol.intern("!#{}");

  /* This stack is used to keep track of the current state viz-a-viz
     quasi-strings. The states are listed below the stack declaration */
  private java.util.Stack quasiStack = new java.util.Stack();

  private static final Integer TOP=new Integer(0);
  private static final Integer STR=new Integer(1);          // inside a "...." string
  private static final Integer QSTR=new Integer(2);     // inside a {....} quasi-string
  private static final Integer HQSTR=new Integer(3);     // inside a #{....#} quasi-string
  private static final Integer QSTRESC=new Integer(4);  // inside a [...] escape of a {...} quasi-string
  private static final Integer HQSTRESC=new Integer(5); // inside a #[...#] escape of a #{...#} quasi-string




  private static Object token(String x) { return new Token(x); }

  boolean isPushedChar = false;
  int     pushedChar = -1;
  int     radix = 10;
  LineNumberReader  in;
  StringBuffer buff = new StringBuffer(8);

  public InputPort (Reader in, boolean keepComments) {
    quasiStack.push(InputPort.TOP);
    if (in instanceof LineNumberReader) 
      this.in = (LineNumberReader) in;
    else this.in = new LineNumberReader(in);
    this.keepComments = keepComments;
  }
  

  /** Construct an InputPort from an InputStream. **/
  public InputPort(InputStream in) {
    this(new InputStreamReader(in), false);
  }

  /** Construct an InputPort from a Reader. **/
  public InputPort(Reader in) {
    this(in, false);
  }

  /** Read and return a Scheme character or EOF. **/
  public synchronized Object readChar() {
    try {
      if (isPushedChar) {
        isPushedChar = false;
        if (pushedChar == -1) return EOF; else return U.toChar((char)pushedChar);
      } else {
        int ch = in.read();
        if (ch == -1) return EOF; else return U.toChar((char)ch);
      }
    } catch (IOException e) {
      E.warn("On input, exception A: " + e);
      return EOF;
    }
  }

  /** Peek at and return the next Scheme character (or EOF).
   * However, don't consume the character. **/
  public synchronized Object peekChar() {
    int p = peekCh();
    if (p == -1) return EOF; else return U.toChar((char)p);
  }

  private Pair pushedTokens = Pair.EMPTY;

  private boolean isPushedToken() {
    return (pushedTokens != Pair.EMPTY);
  }

  /** Push a token back to be re-used later. **/
  private Object pushToken(Object token) {
    pushedTokens = new Pair(token, pushedTokens);
    return token;
  }

  /** Pop off the previously pushed token. **/
  private Object popToken() {
    Object token = pushedTokens.first;
    pushedTokens = (Pair) (pushedTokens.rest);
    return token;
  }

  /** Push a character back to be re-used later. **/
  private int pushChar(int ch) {
    isPushedChar = true;
    return pushedChar = ch;
  }

  /** Pop off the previously pushed character. **/
  private int popChar() {
    isPushedChar = false;
    return pushedChar;
  }

  /** Peek at and return the next Scheme character as an int, -1 for EOF.
   * However, don't consume the character. **/
  private int peekCh() {
    try { return isPushedChar ? pushedChar : pushChar(in.read()); }
    catch (IOException e) {
      E.warn("On input, exception B: " + e);
      return -1;
    }
  }

  public int getLineNumber() { return in.getLineNumber();}

  public Object nextElement() {
    return read();
  }

  public boolean hasMoreElements() {
    try{
      Object token = readToken();
      pushToken(token);
      return (token!=EOF); 
    } catch (IOException e) {
      E.warn("On input, exception C: " + e);
      return false;
    }
  }

  public synchronized Object read() {
    try {
      Object token = readToken();
      if (token instanceof Symbol)  return token; // return lookupGlobal((Symbol) token);
      else if (token instanceof Token) {
	// if (token == OPEN)  return readTail(false);
	if (token == OPEN)  return readList();
	else if (token == QUOTE)  return U.list(Symbol.QUOTE, read());
	else if (token == COMMA)  return U.list(Symbol.UNQUOTE, read());
	else if (token == BACKQUOTE)  return U.list(Symbol.QUASIQUOTE, read());
	else if (token == COMMA_AT)
	  return U.list(Symbol.UNQUOTE_SPLICING, read());
	else if (token == CLOSE)  {
	  E.warn("Extra ) ignored -- line number "+in.getLineNumber());
	  return read(); }
	else if (token == DOT)  {
	  E.warn("Extra . ignored -- line number "+in.getLineNumber());
	  return read(); }
	else return token;
      } else return token;
    } catch (IOException e) {
      E.warn("On input, exception D: " + e);
      return EOF;
    }
  }

  /** Close the port.  Return TRUE if ok. **/
  public Object close() {
    try { this.in.close(); return U.TRUE; }
    catch (IOException e) { return E.error("On input, IOException E: " + e); }
  }

  private Object readList() throws IOException {	// Saw "("
    Object token = readToken();
    if (token instanceof Token) return readListToken((Token) token);
    return readListRest(new Queue(token));
  }

  private Object readListToken(Token token) throws IOException {
    if (token == CLOSE) return Pair.EMPTY; //  "()"
    else if (token == DOT)
      return E.error("'.' not allowed immediately after '('");
    else if (token == EOF) return E.error("EOF during read.");
    else {
      pushToken(token);
      return readListRest(new Queue(read()));
    }
  }

  private Object readListRest(Queue queue) throws IOException {
    int lineNum = getLineNumber();
    // Saw "( x ..."
    while (true) {
      if (Scheme.isInterruptable()) Scheme.interruptCheck();
      Object token = readToken();
      if (token instanceof Token) {
	if (token == CLOSE) return queue.getContent();
	if (token == DOT) {
	  Object result = read();
	  token = readToken(); 
	  if (token != CLOSE)
	    return E.error("Where's the ')'? Got " + token + " after .");
	  queue.getLast().rest = result;
	  return queue.getContent();
	}
	if (token == EOF) return E.error("EOF during read: open paren on line "+lineNum+" not closed.");
        pushToken(token);
	queue.add(read());
      } else queue.add(token);
    }
  }

  /** Returns either a Token or a primitive Scheme type. **/
  private Object readToken() throws IOException {
    Integer qstate = (Integer) quasiStack.peek();

    if (isPushedToken()) {
      Object t = popToken();
      // handle case where a #] was pushed in readSymbolOrNumber
      if (HASH_ESCAPE_CLOSE.equals(t))
	{quasiStack.pop();
	return readString();}
      else return t;
    }

    int ch = (isPushedChar) ? popChar() : in.read();
    while (Character.isWhitespace((char)ch)) ch = in.read();
    // See what kind of non-white character we got
    switch(ch) {
    case '(':  return OPEN;
    case ')':  return CLOSE;
    case '\'': return QUOTE;
    case '`':  return BACKQUOTE;
    case '#':  return readHashToken();
    case '"':  return readString();
    case ',': 
      ch = in.read();
      if (ch == '@') return COMMA_AT;
      else { pushChar(ch); return COMMA; }
    case ';': return readComment(ch);
    case -1:   return EOF;
    case '{':  if (brlsMode) {
      quasiStack.push(InputPort.QSTR);
      pushToken(CURLY);
      isPushedChar=true;
      pushedChar='"';
      return OPEN;
    }
    case ']':  if (brlsMode) {
      if (QSTRESC==qstate) 
	{quasiStack.pop();
	return readString(); }
      else if (HQSTRESC==qstate) {
	ch = in.read();
	if (ch == '#') {
	  quasiStack.pop();
	  return readString();}
      }
    }
              
    default: return readNumberOrSymbol(ch);
    }
  }

  // Comment: skip to end of line and then read next token
  private Object readComment(int ch) throws IOException {
    if (keepComments) return readCommentKeeping(ch);
    else {
      while(ch != -1 && ch != '\n' && ch != '\r')
        ch = in.read();
      return readToken();
    }
  }

  private Object readCommentKeeping(int ch) throws IOException {
    buff.setLength(0);
    int count = 0;
    while (ch == ';') {
      count = count + 1;
      ch = in.read();
    }
    while(ch != -1 && ch != '\n' && ch != '\r') {
      buff.append((char) ch);
      ch = in.read();
    }
    return jscheme.JS.list(Symbol.intern("comment"),
                           new Integer(count),
                           buff.toString());
  }
  

  /*
    Here we read a string or quasi-string or hash-quasi-string
    A string is terminated by a '"' which is not itself quoted.
    A quasi-string is terminated by a '[' or a '}'
    A hash-quasi-string is terminated by a #[ or }#, so we need to check
    whether the [ is preceded by a hash or the } is followed by a hash. The proper way to
    escape such a string is #\[ or }/# ...
  */
  private String readString() {
    Integer qstate = (Integer) quasiStack.peek();
    int ch = 0;
    int lastchar= -1;
    buff.setLength(0);

    if (qstate==QSTR)
      try {
	while (((ch = in.read()) != '}') && (ch != '[') &&  (ch != -1)) {
	  lastchar=ch;
	  buff.append((char) (
			      (ch == '\\') ? 
			      (U.useJavaSyntax?escapechar(in.read()):in.read())
			      : ch));
	}
	if (ch == -1) E.warn("EOF inside of a string.");
	if (ch == '}')  { quasiStack.pop(); pushToken(CLOSE);} 
	if (ch == '[')  { quasiStack.push(QSTRESC);}
	return internStringBuffer(buff);
      } catch (IOException e) { E.warn("On input, IOException G:", e); return ""; }  

    else if (qstate==HQSTR)
      try {
	while (  (!  (((ch = in.read()) == '#') && (lastchar == '}')) ) &&
                 (!  ((ch == '[') && (lastchar =='#'))) &&
		 (ch != -1)) {
	  lastchar=ch;
	  buff.append((char)
		      ((ch == '\\') ? 
		       (U.useJavaSyntax?escapechar(in.read()):in.read()):
		       ch));
	}
	if (ch == -1) E.warn("EOF inside of a string.");
	if (ch == '#')
	  { quasiStack.pop(); pushToken(CLOSE);} 
	if (ch == '[')
	  { quasiStack.push(HQSTRESC);}
        buff.deleteCharAt(buff.length()-1); // get rid of the extra #
	return internStringBuffer(buff);
      } catch (IOException e) { E.warn("On input, IOException H1:", e); return ""; }  
    else
      try {
	while (((ch = in.read()) != '"') && (ch != -1)) {
	  buff.append((char) (
			      (ch == '\\') ? 
			      (U.useJavaSyntax?escapechar(in.read()):in.read())
			      : ch));
	}
	if (ch == -1) E.warn("EOF inside of a string.");
	return internStringBuffer(buff);
      } catch (IOException e) { E.warn("On input, IOException F:", e); return ""; }  
  }
      
  private String internStringBuffer(StringBuffer b) {
    return b.toString().intern();
  }

  private String moveBufToString(StringBuffer b) {
    int L = b.length();
    char[] chars = new char[L];
    if (L>0) {
      b.getChars(0, L, chars, 0);
      b.setLength(0);}
    return  new String(chars);
  }

  /*
    KRA 31MAR01: 4.3% of loading elf/basic.scm was spent thowing
    NumberFormatException i think because + and - are commonly used
    symbols.  So added length test.

    KRA 06APR03: This did not work for #xff. so add tests for each
    radix.
  */
  private boolean maybeNumber(int c, String buff,int radix) {
    return
      (radix == 10 &&  (c >= '0' && c <= '9' ||
			((c == '.' || c == '+' || c == '-') &&
			 buff.length() > 1))) ||
      (radix == 16 && ((c >= '0' && c <= '9') ||
		       (c >= 'a' && c <= 'f') ||
		       (c >= 'a' && c <= 'A'))) ||
      (radix == 8 && (c >= '0' && c <= '7')) ||
      (radix == 2 && (c >= '0' && c <= '1'));
  }

  /*
    read a number or a symbol
    if in quasi-string-escape mode, then a ] delimits a number or symbol
    if in hash-quasi-string-escape mode, then a #] delimits a number or symbol
  */
  private Object readNumberOrSymbol(int ch) throws IOException {
    Integer qstate = (Integer) quasiStack.peek();
    buff.setLength(0);
    int c = ch; // save the first character
    boolean foundDelimiter = false;

    do { // accumulate characters up to the next delimiter
      buff.append((char)ch);
      ch = in.read();
      foundDelimiter=isDelimiter(ch);

      /* check for a ]# delimiting a number or symbol */
      if ((ch==']') && (qstate==HQSTRESC)) {
	ch=in.read();
	if (ch=='#') 
	  {foundDelimiter=true;
	  pushToken(HASH_ESCAPE_CLOSE);
	  ch=' ';
	  continue;}
	else {
	  buff.append(']');
	}
      }
                   
      if ((ch==']') && (qstate == QSTRESC)) {
	foundDelimiter=true;
      }

      /* this next if expression handles the quasi-string exception
         which allows [] to appear within Symbols in quasi-string escapes */
      if ((ch=='[')&&(qstate==QSTRESC)) {  
	buff.append((char)ch);
	ch=in.read();
	foundDelimiter = isDelimiter(ch) && (ch != ']');
      }

    } while (! foundDelimiter);
  pushChar(ch);
  if (c == '.' && buff.length() == 1) return DOT;
  String tok = moveBufToString(buff);
  // Try potential numbers, but catch any format errors.
  try {
    if (maybeNumber(c, tok, radix)) {
      Object it = stringToNumber(tok, radix);
      if (it instanceof Number) return it;
    }} catch(NumberFormatException e) {
      return Symbol.intern(tok);
    }
  return Symbol.intern(tok);
  }
  	      
  public static Object schemeStringToNumber(String tok, int rdx) {
    try { return U.toNum(Long.parseLong(tok, rdx)); }
    catch (NumberFormatException e) { 
      try {
	if (rdx!=10)
	  return U.FALSE;
	else return new Double(tok); }
      catch (NumberFormatException e2)
	{ return U.FALSE; } 
    }
  }


  public static Object stringToNumber(String tok, int rdx) {
    if (rdx == 10) {
      if (U.useJavaSyntax) {
	try { return readWholeNumber(tok);}
	catch (NumberFormatException e1) { 
	  try { return readFloatingPoint(tok);}
	  catch (NumberFormatException e3) { return U.FALSE;}
	}}
      else {
	try { return U.toNum(Long.parseLong(tok, rdx)); }
	catch (NumberFormatException e) { 
	  try { return new Double(tok); }
	  catch (NumberFormatException e2) { return U.FALSE; } }
      }
    } else return U.toNum(Long.parseLong(U.stringify(tok, false), rdx));
  }

  /* attempt to parse a long or an int in Java syntax
     this currently accepts +---+013LLBSBSL --> -11L
     we may want to disallow multiple signs and multiple type characters
  */
  public static Number readWholeNumber(String s) throws NumberFormatException{
    long n;
    if (s.endsWith("l") || s.endsWith("L"))
      return new Long(readWholeNumber(s.substring(0,s.length()-1))
		      .longValue());
    if (s.endsWith("b") || s.endsWith("B"))
      return new Byte(readWholeNumber(s.substring(0,s.length()-1))
		      .byteValue());
    if (s.endsWith("s") || s.endsWith("S"))
      return new Short(readWholeNumber(s.substring(0,s.length()-1))
		       .shortValue());
    else if (s.startsWith("+") && (s.length() > 1))
      return readWholeNumber(s.substring(1,s.length()));
    else if (s.startsWith("-") && (s.length() > 1))
      return negate(readWholeNumber(s.substring(1,s.length())));
//     else if (s.startsWith("0x")) 
//       n = Long.parseLong(s.substring(2,s.length()),16);
//     else if ((s.startsWith("0")) && (s.length()>1))
//       n = Long.parseLong(s.substring(1,s.length()),8);
    else n = Long.parseLong(s,10);
    return U.toNum(n);
  }

  public static Number negate(Number n) { 
    if (n instanceof Integer) return U.toNum(-n.intValue());
    else if (n instanceof Long) return new Long(-n.longValue());
    else if (n instanceof Float) return new Float(-n.floatValue());
    else if (n instanceof Double) return new Double(-n.doubleValue());
    else { // this should never happen!
      E.warn("ERROR in Inputport.negate called with "+n.getClass()+" returning " + n);
      return n;
    }
  }

  /* attempt to read a float or double in Java syntax */
  public static Number readFloatingPoint(String s)
    throws NumberFormatException{
    if (s.endsWith("f") || s.endsWith("F"))
      return new Float(readFloatingPoint(s.substring(0,s.length()-1)).floatValue());
    else if (s.endsWith("d") || s.endsWith("D"))
      return new Double(readFloatingPoint(s.substring(0,s.length()-1)).doubleValue());
    else if (s.startsWith("+")&& (s.length()>1))
      return readFloatingPoint(s.substring(1,s.length()));
    else if (s.startsWith("-")&&(s.length() > 1))
      return negate(readFloatingPoint(s.substring(1,s.length())));
    else if (s.startsWith("0")&& (s.length()>1) && (Character.isDigit(s.charAt(1))))
      throw 
	(new NumberFormatException
	 ("floating point starting with 0 is either an octal or 0.ddd"));
    else return new Double(s);
  }

  private Object readHashToken() throws IOException {
    int ch;
    Object token = null;  
    switch (ch = in.read()) {
    case 't': case 'T': return U.TRUE;
    case 'f': case 'F': return U.FALSE;
    case 'n': {			      // #null
      pushChar(ch);
      token=readToken();
      if (token == Symbol.NULL) return null;
      else E.warn("illegal syntax #"+token+" ignored");
      return readToken();
    }
    case '(':			//  #(...) vector.
      pushChar('(');
      return U.listToVector(read());
    case '\\':			// #\...
      ch = in.read();
      if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N') {
        pushChar(ch);
        token = readToken();
        /* at this point token must be one of 
	   SPACE, space, NEWLINE, newline, s, S, n, or N
           or we have an error, and we warn the user and ignore the token
        */
	if ((token == SPACE_UC) || (token==SPACE_LC))  return U.toChar(' ');
        else if ((token == NEWLINE_UC) || (token==NEWLINE_LC)) return U.toChar('\n');
        else if (token==CHAR_s) return U.toChar('s');
        else if (token==CHAR_S) return U.toChar('S');
        else if (token==CHAR_n) return U.toChar('n');
        else if (token==CHAR_N) return U.toChar('N');
        else {
          E.warn("illegal syntax #"+token+" ignored");
          return readToken();
        }
      } else return U.toChar((char)ch);
    case '\'': 
      if (U.useJavaSyntax) {
        ch = in.read();
        Character x;
        if (ch != '\\')  x= new Character((char) ch);
        else x = new Character(escapechar(in.read()));
        ch = in.read();
        if (ch != '\'') E.warn("character syntax is #'C', or #\\C, not #'C"+((char)ch));
        return x;
      }

      // start of hashQuasiString #{ .... #} with hash escapes #[...#]
      // implement by converting to (!#{} A B ... Z)
    case '{': 
      if (brlsMode) {
	quasiStack.push(InputPort.HQSTR);
	pushToken(HASH_CURLY);
	isPushedChar=true;
	pushedChar='"';
	return OPEN;
      }
      // Unix #! treated as comment.
    case '!': return readComment(ch);

      // else fall through to the default
    default: 
      if (U.useJavaSyntax) {
	E.warn("#" + ((char)ch) + " not recognized, ignored."); 
	return readToken();}
      else 
	switch(ch) {
	case 'e': return U.toNum(U.toInt(readToken()));
	case 'i': return U.toNum(U.toReal(readToken()));
	case 'd': return readToken();
	case 'b': case 'o': case 'x':
	  return readHashNumber(ch == 'b' ? 2 : ch == 'o' ? 8 : 16);
	default: 
	  E.warn("#" + ((char)ch) + " not recognized, ignored."); 
	  return readToken();
	}
    }
  }
		     
  private Object readHashNumber(int radix) throws IOException {
    synchronized (this) { // synchonize to potect radix
      this.radix = radix;
      Object token = readToken();  // should be readNumberOrSymbol(...);
      this.radix = 10; 
      return token;
    }
  }

  private char escapechar(int c) throws IOException {
    switch (c) {
    case  'b': return '\b';
    case  't': return '\t';
    case  'n': return '\n';
    case  'f': return '\f';
    case  'r': return '\r';
    case  '"': return '"';
    case '\'': return '\'';
    case '\\': return '\\';
    case  '0': return new Character((char) Integer.parseInt(
							    ("" + (char)in.read() + (char)in.read()),
							    8)).charValue(); 
    case  'u': return new Character((char) Integer.parseInt(
							    ("" + (char)in.read() + (char)in.read() + (char)in.read() + (char)in.read()),
							    16)).charValue();  
    case '[': case ']': case '{': case '}': return (char) c;
	
    default: {
      E.warn("Expected a Java escape sequence for a character, found " + ((char) c) + " with ascii code "+c);
      return (char) c;
    }
    }
  }

  private boolean isDelimiter(int ch) {
    switch (ch) {
    case -1: case '(': case ')': case '\'': case ';': case ',': 
    case '"': case '`': case ' ': case '\t': case '\n': 
      return true;
    default: return Character.isWhitespace((char)ch);
    }
  }


  }

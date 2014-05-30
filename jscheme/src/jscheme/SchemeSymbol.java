package jscheme;

/**
  A SchemeSymbol is an object that can be defined or not.
  SchemeSymbols are created using the REPL.internSchemeSymbol(string)
  and are initially undefined. 
  Its value can be queried and set.
  Getting the value of an undefined SchemeSymbol throws an error.

  method. Getting
 **/
public interface SchemeSymbol extends java.io.Serializable {

    /**
      SchemeSymbols are initially undefined.
     **/
    public boolean isDefined();

    /**
      Getting the value of an undefined SchemeSymbol throws a SchemeException
     **/
    public Object getGlobalValue();

    /**
      The value can be set to anything
     **/
    public Object setGlobalValue(Object newval);
}

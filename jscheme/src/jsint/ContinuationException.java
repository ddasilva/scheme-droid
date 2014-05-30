package jsint;
/**
   A continuationException is thrown by a continuation.  See U.callCC.
**/

public class ContinuationException extends RuntimeException {
  /** Clever performance trick i found in
      http://docs.msdnaa.net/ark_new/Webfiles/WhitePapers/Babel01/bab12.pdf
  **/
  public Throwable fillInStackTrace() {
    return this;
  }
}

  

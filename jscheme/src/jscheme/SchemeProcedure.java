package jscheme;

/**
  A SchemeProcedure is an object that can be applied to an array of
  objects or a SchemePair of objects. It is also runnable (so it
  can be used in threads)

 **/
public interface SchemeProcedure extends Runnable, java.io.Serializable {

  public void run();
  public abstract Object apply(Object[] args);
  public Object apply(SchemePair args);

}

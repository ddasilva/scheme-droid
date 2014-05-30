package jsint;

  /** A queue, q, is a Pair (cons ,last ,content), see Peter's, PAIP book.

      <p> This lets .add() be written without a null comparison, but
      uses an extra Pair.

      <p>Used by InputPort.readTail and U.append.
  **/
public class Queue {
  private Pair q;

  /** Create an empty Queue. **/
  public Queue() {
    q = new Pair(null, Pair.EMPTY);
    q.first = q;
  }

  /** Create a Queue containing the single element, item. **/
  public Queue(Object item) {
    this();
    this.add(item);
  }

  /** Get the last Pair of the Queue's content. **/
  public Pair getLast   () { return ((Pair) q.first); }

  /** Get the Queue's content as a Pair list. **/
  public Object getContent() { return q.rest; }

  /** Add an item to the end of the Queue. **/
  public Queue add(Object item) {
    Pair last = new Pair(item, Pair.EMPTY);
    this.getLast().rest = last;
    q.first = last;
    return this;
  }

  /** Remove an item from the front of the Queue. **/
  public Queue remove() {
    if (U.isPair(q.rest)) q.rest = U.rest(q.rest);
    if (! U.isPair(q.rest)) q.first = q;
    return this;
  }

  /** Return the first item in the Queue, or null if the Queue is empty. **/
  public Object front() {
    Object c = this.getContent();
    if (U.isPair(c)) return U.first(c);
    else return null;
  }

  public Object pop() {
    Object result = this.front();
    this.remove();
    return result;
  }
}

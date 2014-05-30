package dclass;
public class Item {
  public Object item;
  public Item() {}
  public Item(Object item) { this.item = item; }
  public String toString () {
    return "{" + this.getClass().getName() + " " + item + "}";
  }
}

package jlib;

import java.awt.*;
import jsint.*;


/**
 *  This class represents a Frame in which the window
 *  which responds "properly" to window closing events.
 *  It also allows one to handle events in the Frame by
 *  specifying callbacks using Scheme closures.
 *
 *  This is needed when working with the Java 1.0 event
 *  model which does not have event listeners.
 *
 *  @author Timothy J. Hickey, tim@cs.brandeis.edu http://www.cs.brandeis.edu/~tim 
 */


public class EventFrame extends java.awt.Frame {

    public Procedure handler;

    public EventFrame() {
	super();
    }

    public EventFrame(String title) {
	super(title);
    }

  public void addEventHandler(Procedure callback) {
     handler = callback;
  }

  public void update(Graphics g) {
    paint(g);
  }

  public boolean handleEvent(Event e) {

    if (handler != null) {
       return (null != handler.apply(U.list(e)));
    }
    else if (e.id==Event.WINDOW_DESTROY) {
       hide(); dispose(); return true;
    }
    else return super.handleEvent(e);
  }
}


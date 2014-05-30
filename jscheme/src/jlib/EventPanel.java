package jlib;
import jsint.*;
import java.awt.*;

/**
 *   This class represents a panel which executes closures to handle
 *   events in the Java 1.0 event model. 
 *   @author Timothy J. Hickey, tim@cs.brandeis.edu http://www.cs.brandeis.edu/~tim 
 **/


public class EventPanel extends Panel {
    public Procedure handler;
    public Component C;
    public final int GENERAL=0,ACTION=1,MOUSE=2;
    int eventmask=ACTION;

    public EventPanel(Component C, Procedure handler) {
	super();
        this.C=C; this.handler=handler;
        this.add(C);
    }

    public EventPanel(Component C, int eventmask, Procedure handler) {
	super();
        this.C=C; this.handler=handler;
        this.add(C);
        this.eventmask=eventmask;
    }

    public void update(Graphics g) { paint(g); } // don't let Java blank the screen

    public boolean handleEvent(Event e) {
        if (eventmask==GENERAL) {
            Object x = handler.apply(U.list(e));
            return !(Boolean.FALSE).equals(x);
	}else return super.handleEvent(e);
    }

    public boolean action(Event e, Object what) {
        if (eventmask==ACTION) {
            Object x = handler.apply(U.list(e));
            return !(Boolean.FALSE).equals(x);
	}else return false;
    }

    public boolean mouseDown(Event e, int x1, int y1) {
        if (eventmask==MOUSE) {
            Object x = handler.apply(U.list(e));
            return !(Boolean.FALSE).equals(x);
	}else return false;
    }

    public boolean mouseUp(Event e, int x1, int y1) {
        if (eventmask==MOUSE) {
            Object x = handler.apply(U.list(e));
            return !(Boolean.FALSE).equals(x);
	}else return false;
    }

    public boolean mouseDrag(Event e, int x1, int y1) {
        if (eventmask==MOUSE) {
            Object x = handler.apply(U.list(e));
            return !(Boolean.FALSE).equals(x);
	}else return false;
    }

    public boolean mouseMove(Event e, int x1, int y1) {
        if (eventmask==MOUSE) {
            Object x = handler.apply(U.list(e));
            return !(Boolean.FALSE).equals(x);
	}else return false;
    }

    public boolean mouseEnter(Event e, int x1, int y1) {
        if (eventmask==MOUSE) {
            Object x = handler.apply(U.list(e));
            return !(Boolean.FALSE).equals(x);
	}else return false;
    }

    public boolean mouseExit(Event e, int x1, int y1) {
        if (eventmask==MOUSE) {
            Object x = handler.apply(U.list(e));
            return !(Boolean.FALSE).equals(x);
	}else return false;
    }

}





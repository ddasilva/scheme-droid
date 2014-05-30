package jlib;
import jsint.*;
import java.awt.*;

/**
 *   This class represents a panel which executes closures to handle
 *   events in the Java 1.0 event model. 
 *   @author Timothy J. Hickey, tim@cs.brandeis.edu http://www.cs.brandeis.edu/~tim 
 **/


public class SchemeCanvas extends Panel {
    public Image buffer;
    public Graphics bufferg;
    public int width,height;
    int bufferwidth,bufferheight;
    public Procedure paintHandler;
    Dimension size;

    public SchemeCanvas(int w, int h) {
	super();
        width=bufferwidth=(w>0)?w:1; 
        height=bufferheight=(h>0)?h:1;
        size = new Dimension(w,h);
    }


    public Dimension preferredSize() {return size;}

    public void update(Graphics g) { paint(g); } // don't let Java blank the screen

    /* if new window needs larger buffer then expand image buffer */
    void resizeSchemeCanvas(Dimension s) {
        int w=s.width;
        int h=s.height;
        boolean resize= ((w>bufferwidth)||(h>bufferheight));
        width=w; height=h;
        bufferwidth  = (w<bufferwidth) ?bufferwidth: w;
        bufferheight = (h<bufferheight)?bufferheight:h;
	if (resize) {
	    Image buffer2 = buffer;
	    // Graphics bufferg2 = bufferg;
            buffer = this.createImage(bufferwidth,bufferheight);
            bufferg = buffer.getGraphics();
            bufferg.drawImage(buffer2,0,0,this);
	}
    }

    public void paint(Graphics g) {
        if (buffer==null) buffer = this.createImage(width,height);
        if (bufferg==null) bufferg = buffer.getGraphics();
        if (!(size.equals(this.size()))) resizeSchemeCanvas(this.size());

        if (paintHandler != null) {
            Object x = paintHandler.apply(U.list(bufferg));
            if (buffer!=null)
               g.drawImage(buffer,0,0,this);
	}else{
            if (buffer!=null)
               g.drawImage(buffer,0,0,this);
	}
    }

}





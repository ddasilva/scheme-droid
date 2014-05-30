package jlib;
import java.awt.*;

public class EditArea extends TextArea {

    public static final int inExpr=0;
    public static final int topLevel=1;
    public static final int tooManyParens=2;
    public static final int inQuote=3;
    public static final int inComment=4;


    public static boolean j11enabled,netscapebug;
    private int state;
    public static boolean matchParens=false;
    public static boolean computeCurrentExpr=false;
    public String currentExpr="";
    public jsint.Procedure matchHandler;

    public EditArea(int r, int c) {
	super(r,c);
        try {(java.awt.TextArea.class)
	       .getMethod("getCaretPosition",new Class[]{}); j11enabled=true;}
        catch(Exception e) {j11enabled=false;}
        netscapeBugTest();
    }

    public boolean netscapeBugTest(){
	// test for Netscape bug on Internet Explorer
        netscapebug = 
	    (   ("45.3".equals(
                  System.getProperty("java.class.version")))
             && ("Windows NT".equals(
                  System.getProperty("os.name")))
             && ("Netscape Communications Corporation".equals(
                  System.getProperty("java.vendor")))
		);
        return netscapebug;
    }

    public boolean keyUp(Event e, int k) {
	    int pos,pos2=0;
            if (j11enabled)    pos=this.getCaretPosition();
            else               pos = this.getSelectionEnd();
	    char[] chars = this.getText().toCharArray();

	    int i;
	    int parencount; 

            if ((!computeCurrentExpr) && (!matchParens)) return super.keyUp(e,k);

	    // first we go through the area removing all comments and quotes
            int j=0;
            while(j<pos) {
                if (netscapebug && (chars[j]=='\n')) pos--;

		if (chars[j]==';') { pos2=j;
		    do {
			if ((chars[j]=='(')||(chars[j]==')')) chars[j]='|'; 
                        if (netscapebug && (chars[j]=='\n')) pos--;
                        j++; }
                    while ((j<pos)&&(chars[j]!='\n'));
                    if(j==pos) {
                  	currentExpr = (this.getText()).substring(pos2,pos);
			if (matchHandler!= null) matchHandler.apply(jsint.U.list(
                            this,currentExpr,new Integer(inComment)));
			return super.keyUp(e,k);}
		}
                else if (chars[j]=='"') { pos2=j;
		    boolean val;
		    do {
			if ((chars[j]=='(')||(chars[j]==')')) chars[j]='|'; 
                        if ((chars[j]=='\\')&&(j<pos)&&(chars[j+1]=='"'))  chars[j+1]='_';
                        if ((chars[j]=='\\')&&(j<pos)&&(chars[j+1]=='\\'))  chars[j+1]='_';
                        if (netscapebug && (chars[j]=='\n')) pos--;
                        j++;
		    }
		    while ((j<pos)&&(chars[j]!='"'));
                    if(j==pos) {
                  	currentExpr = (this.getText()).substring(pos2,pos);
			if (matchHandler!= null) matchHandler.apply(jsint.U.list(
                            this,currentExpr,new Integer(inQuote)));
			return super.keyUp(e,k);}
                    else j++;
		}
                else  j++;
	    }



	// find first paren of current expression
        int parenbias = (pos>0)?((chars[pos-1]==')')?1:0):0;
        i=pos-1;
        parencount=0;
        if (i>=0) do {
	    if (chars[i]==')') parencount++;
	    else if (chars[i]=='(') parencount--;
	    i--;}
        while ((i>=0) && (parencount>=parenbias));

	currentExpr = (this.getText()).substring(i+1,pos);
	if (matchParens && (k==41)) flashMatch(i,pos,parencount);

        if ((i== -1) && (parencount > 0)) state = tooManyParens;
        else state = inExpr;
	if (matchHandler!= null) 
	    matchHandler.apply(jsint.U.list(this,currentExpr,new Integer(state)));

	return super.keyUp(e,k);
    }

    private void flashMatch(int i, int pos, int parencount) {
		if ((i == -1) && (parencount > 0)) {
		    for (int n=1; n<5; n++) {
			if (j11enabled) {
			    this.setSelectionStart(i+1); 
			    this.setSelectionEnd(pos);
			}else  this.select(i+1,pos);
			try {Thread.sleep(100);} catch (Exception ee) {;}
			if (j11enabled)  this.setSelectionStart(pos);
			else             this.select(pos,pos);
			try {Thread.sleep(100);} catch (Exception ee) {;}}
		} else {
		if (matchParens) {
		    if (j11enabled) {
                       this.setSelectionStart(i+1); 
                       this.setSelectionEnd(pos);
                    }else  this.select(i+1,pos);
                   try {Thread.sleep(250);} catch (Exception ee) {;}
		    if (j11enabled)  this.setSelectionStart(pos);
                    else             this.select(pos,pos);}
		}

    }

}


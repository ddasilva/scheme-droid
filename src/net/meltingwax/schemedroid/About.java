package net.meltingwax.schemedroid;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

/**
 * An activity that displays an about screen.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 */
public class About extends Activity {
	private static String VERSION = "0.2";
	private static String ACTIVITY_TITLE = "Scheme Droid About";
	private TextView aboutText;
	
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        aboutText = new TextView(this);
        aboutText.setText("Scheme Droid v" + VERSION + "\n"
    				+ "\n"
    				+ "Assembled by meltingwax (Daniel da Silva). However "
    				+ "real credit for the interpretter goes to the JScheme "
    				+ "authors.\n"
    				+ "\n"
    				+ "Send comments and feedback to:\n"
    				+ "daniel@meltingwax.net");
        
        setContentView(aboutText);
        setTitle(ACTIVITY_TITLE);
    }
}

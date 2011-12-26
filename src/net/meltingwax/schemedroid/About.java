package net.meltingwax.schemedroid;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * An activity that displays an about screen.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 */
public class About extends Activity {
	private static String VERSION = "0.2";
	private static String ACTIVITY_TITLE = "Scheme Droid About";	
	
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        LinearLayout parent = new LinearLayout(this);
        parent.setOrientation(LinearLayout.VERTICAL);
        parent.setVerticalScrollBarEnabled(true);               
                
        TextView aboutText = new TextView(this);
        aboutText.setText("Scheme Droid v" + VERSION + " http://code.google.com/p/scheme-droid/ \n"
        		 	+ jsint.Version.VERSION + "\n"
    				+ "\n"
    				+ "Assembled by meltingwax (Daniel da Silva). However "
    				+ "real credit for the interpretter goes to the JScheme "
    				+ "authors.\n"
    				+ "\n"
    				+ "Send comments and feedback to:\n"
    				+ "daniel@meltingwax.net\n");
        
        parent.addView(aboutText);
       
        Button projectPageButton = new Button(this);
        projectPageButton.setText("Open Project Page");
        projectPageButton.setOnClickListener(new View.OnClickListener() {		
			public void onClick(View v) {
				Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse("http://code.google.com/p/scheme-droid/"));
				startActivity(browserIntent);

			}
        });
        parent.addView(projectPageButton);
        
        Button marketPageButton = new Button(this);
        marketPageButton.setText("Market Page");
        marketPageButton.setOnClickListener(new View.OnClickListener() {		
			public void onClick(View v) {
				Intent marketIntent = new Intent(Intent.ACTION_VIEW, Uri.parse("market://details?id=net.meltingwax.schemedroid"));
				startActivity(marketIntent);

			}
        });
        parent.addView(marketPageButton);
        
        setContentView(parent);
        setTitle(ACTIVITY_TITLE);
    }
}

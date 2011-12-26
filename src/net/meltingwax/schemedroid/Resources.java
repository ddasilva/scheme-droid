package net.meltingwax.schemedroid;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Button;


public class Resources extends Activity {		
	private String ACTIVITY_TITLE = "Scheme Droid Links";
	
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        LinearLayout parent = new LinearLayout(this);
        parent.setOrientation(LinearLayout.VERTICAL);
        parent.setVerticalScrollBarEnabled(true);               
        
        TextView linksText = new TextView(this);
        linksText.setText("\nLinks (open in browser)\n\n");        
        parent.addView(linksText);
        
        addLinkButtonToLinearLayout(parent, "Calling Java Classes using Scheme Droid", "http://jscheme.sourceforge.net/jscheme/doc/javaprimitives.html");
        addLinkButtonToLinearLayout(parent, "SICP Table of Contents", "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start");
        addLinkButtonToLinearLayout(parent, "R5RS Scheme Standard Table of Contents", "http://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-2.html#%_toc_start");
        addLinkButtonToLinearLayout(parent, "Reference Manual for Jscheme", "http://jscheme.sourceforge.net/jscheme/doc/refman.html");
        addLinkButtonToLinearLayout(parent, "Schemers.org", "http://schemers.org/");
        
        setContentView(parent);
        setTitle(ACTIVITY_TITLE);    
    }
    
    private void addLinkButtonToLinearLayout(LinearLayout parent, String title, final String url) {
        Button button = new Button(this);
        button.setText(title);
        button.setOnClickListener(new View.OnClickListener() {			
			public void onClick(View v) {
				Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
				startActivity(browserIntent);
			}
		});
        parent.addView(button);
    }
}

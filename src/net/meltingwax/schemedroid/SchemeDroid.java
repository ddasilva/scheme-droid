package net.meltingwax.schemedroid;

import android.app.Activity;
import android.app.AlertDialog;
import android.os.Bundle;
import android.view.Display;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import jscheme.JScheme;


/**
 * An Android App Wrapper to JScheme.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 */
public class SchemeDroid extends Activity {
	private static String VERSION = "0.2";
	private static int EVAL_BUTTON_WIDTH = 60;
	
	private JScheme js;
	private TextView console;
	private EditText entry;
	
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        /*
         * Initialize JScheme 
         */
        js = new JScheme();
        
        /*
         * Initialize the View 
         */
        Display display = getWindowManager().getDefaultDisplay();
        
        ScrollView parentScroller = new ScrollView(this);
        
        LinearLayout parentLayout = new LinearLayout(this);
        parentLayout.setOrientation(LinearLayout.VERTICAL);
        parentLayout.setVerticalScrollBarEnabled(true);
                
        console = new TextView(this);
        parentLayout.addView(console);
        
        // The bottom panel that holds the input entry and eval button
        LinearLayout bottomPanel = new LinearLayout(this);
        bottomPanel.setOrientation(LinearLayout.HORIZONTAL);
        
        entry = new EditText(this);
        entry.setWidth(display.getWidth() - EVAL_BUTTON_WIDTH);
        entry.setOnKeyListener(new View.OnKeyListener() {
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_ENTER) {
					processEntry();
					return true;
				}				
				return false;
			}
		});
        bottomPanel.addView(entry);
        
        Button evalButton = new Button(this);
        evalButton.setWidth(EVAL_BUTTON_WIDTH);
        evalButton.setText("Eval");
        evalButton.setOnClickListener(new View.OnClickListener() {
			public void onClick(View v) {
				processEntry();
			}
		});
        bottomPanel.addView(evalButton);
        
        parentLayout.addView(bottomPanel);
 
        parentScroller.addView(parentLayout);
        setContentView(parentScroller);
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
    	super.onCreateOptionsMenu(menu);    	
    	menu.add("Reset");    	
    	menu.add("About");
    	return true;
    }
    
    public boolean onOptionsItemSelected(MenuItem item) {
    	// I am pretty sure item.getTile().equal(x) is not the proper way
    	// to do this, but there have been worse sins committed.
    	if (item.getTitle().equals("Reset")) {
    		js = new JScheme();
    		console.setText("");
    		entry.setText("");
    	}
    	else if (item.getTitle().equals("About")) {
    		AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(this);
    		dialogBuilder.setMessage("Scheme Droid v" + VERSION + "\n"
    				+ "\n"
    				+ "Assembled by Daniel da Silva (meltingwax). However "
    				+ "real credit for the interpretter goes to the JScheme "
    				+ "authors.\n"
    				+ "\n"
    				+ "Send comments and feedback to daniel@meltingwax.net");
    		dialogBuilder.setCancelable(true);
    		dialogBuilder.create().show();
    	}
    	
    	return true;
    }
    
    /**
     * Processes the code in the entry EditText and updates the view. 
     */
    public void processEntry() {
    	String code = entry.getText().toString();
    	
    	if (! code.trim().equals("")) {
    		String resp;
    		
    		try {
    			resp = js.eval(code).toString();
    		} catch (jscheme.SchemeException e) {
    			resp = e.getMessage();
    		} catch (jsint.BacktraceException e) {
    			resp = e.getMessage();
    		} catch (Exception e) {
    			resp = "Generic Error: " + e.toString();
    		}
    		
    		console.append("\n>" + code + "\n" + resp);
    		
    		entry.setText("");
    	}
    }
}
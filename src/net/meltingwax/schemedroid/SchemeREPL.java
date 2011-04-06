package net.meltingwax.schemedroid;

import android.app.Activity;
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
 * Read Eval Print Loop activity.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 */
public class SchemeREPL extends Activity {
	private static String ACTIVITY_TITLE = "Scheme Droid REPL";
	private static int EVAL_BUTTON_WIDTH = 60;
	
	private JScheme js;
	private TextView console;
	private EditText entry;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        /*
         * Initialize JScheme 
         */
        js = new JScheme();
        
        /*
         * Initialize the UI 
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
        setTitle(ACTIVITY_TITLE);
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
    	super.onCreateOptionsMenu(menu);    	
    	menu.add("Reset");    	
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
    	
    	return true;
    }
    
    /**
     * Processes the code in the entry EditText and updates the UI. 
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
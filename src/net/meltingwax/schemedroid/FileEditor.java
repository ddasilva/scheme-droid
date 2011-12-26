package net.meltingwax.schemedroid;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.net.Uri;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;


public class FileEditor extends Activity {
	private File file;
	private EditText console;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
    	super.onCreate(savedInstanceState);
    	
        console = new EditText(this);
        
        Intent intent = getIntent();
        if (intent != null) {
        	Uri uri = intent.getData();
        	if (uri.getScheme().equals("file")) {        		
        		file = new File(uri.getPath());                
                final long length = file.length();                
                byte[] fileData = new byte[(int)length];
                
                try {
                    InputStream fileStream = new FileInputStream(file);
                    fileStream.read(fileData);
                    fileStream.close();
                }
                catch (FileNotFoundException ex) {
                	console.append(ex.toString());
                }
                catch (IOException ex) {
                	console.append(ex.toString());

                }
                
                console.append(new String(fileData));
        	}
        }        
        setContentView(console);
        
        setTitle(intent.getData().toString());
    }
    
    

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
    	super.onCreateOptionsMenu(menu);    	
    	menu.add("Save");

    	return true;
    }
    
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {  	
    	if (item.getTitle().equals("Save")) {
    		OutputStream out = null;
    		try {
    			out = new FileOutputStream(file);
    		}
    		catch (FileNotFoundException ex) {}
    		
    		try {    		
    			out.write(console.getText().toString().getBytes());
    		} catch (IOException ex) {}
    	}
    	
    	return true;
    }      
}

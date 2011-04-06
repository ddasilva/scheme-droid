package net.meltingwax.schemedroid;

import java.util.HashMap;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.webkit.WebView;
import android.webkit.WebViewClient;


/**
 * The main Scheme Droid home screen activity.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 */
public class SchemeDroid extends Activity {
    private WebView mWebView;
	private String homeHtmlPath = "file:///android_asset/home.html";
	
	private class HomeWebView extends WebViewClient {		
		@Override
		@SuppressWarnings("unchecked")
	    public boolean shouldOverrideUrlLoading(WebView view, String url) {	    	
			HashMap<String, Class> map = new HashMap<String, Class>();
			map.put("schemedroid://about", About.class);
			map.put("schemedroid://help", SchemeREPL.class);
			map.put("schemedroid://new", SchemeREPL.class);
			map.put("schemedroid://open", SchemeREPL.class);			
			map.put("schemedroid://repl", SchemeREPL.class);
			
			if (map.containsKey(url)) {
                Intent myIntent = new Intent(view.getContext(), map.get(url));
                startActivity(myIntent);
                return true;
			}

			return false;
	    }
	}
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
    	super.onCreate(savedInstanceState);
    	mWebView = new WebView(this);
    	setContentView(mWebView);
        mWebView.setWebViewClient(new HomeWebView());
        
    	if (savedInstanceState != null) {
    		mWebView.restoreState(savedInstanceState);
    	} else {
    		mWebView.loadUrl(homeHtmlPath);
    	}
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
    	super.onCreateOptionsMenu(menu);    	
    	// menu.add("Reset");
    	return true;
    }
    
    public boolean onOptionsItemSelected(MenuItem item) {
    	return true;
    }
}
package net.meltingwax.schemedroid;

import java.util.HashMap;
import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
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
	private static final int PICK_REQUEST_CODE = 0;
	
	
	private class HomeWebView extends WebViewClient {		
		
		@Override
		@SuppressWarnings("rawtypes")
	    public boolean shouldOverrideUrlLoading(WebView view, String url) {	    	
			HashMap<String, Class> map = new HashMap<String, Class>();
			map.put("schemedroid://about", About.class);
			map.put("schemedroid://help", SchemeREPL.class);
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
    
    /**
     * Get a file URI from the FileBrowser activity.
     */
    protected void onActivityResult(int requestCode, int resultCode, Intent intent) {
    	if (requestCode == PICK_REQUEST_CODE && resultCode == RESULT_OK) {
    		Uri uri = intent.getData();
    		if (uri != null) {
			    String path = uri.toString();
			    if (path.toLowerCase().startsWith("file://")) {
			    	//path = (new File(URI.create(path))).getAbsolutePath();			    	
			    	//android.util.Log.d("DEBUG", Uri.parse(path).toString());
			    	Intent myIntent = new Intent(Intent.ACTION_VIEW, uri, SchemeDroid.this, FileEditor.class);			    	
			    	startActivity(myIntent);
			    	//loadFile(Uri.parse(path), "text/html");
			    }
		    }
	    }
    }

}
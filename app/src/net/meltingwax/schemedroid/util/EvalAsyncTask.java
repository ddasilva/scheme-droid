package net.meltingwax.schemedroid.util;

import net.meltingwax.schemedroid.activity.fragment.ReplFragment;
import jscheme.JScheme;
import jsint.Evaluator.CancelIndicator;
import android.os.AsyncTask;
import android.view.View;
import android.widget.EditText;
import android.widget.ProgressBar;
import android.widget.TextView;

public class EvalAsyncTask extends AsyncTask<String, Void, String> implements CancelIndicator {

	private JScheme js;
	private TextView console;
	private ProgressBar runningIndicator;
	private EditText entry;
	private ReplFragment fragment;
	
	public EvalAsyncTask(ReplFragment fragment) {
		this.js = fragment.getJScheme();
		this.console = fragment.getConsole();
		this.runningIndicator = fragment.getRunningIndicator();
		this.entry = fragment.getEntry();
		this.fragment = fragment;
	}

	protected void onCancelled(String result) {
		onPostExecute(result);
	}

	/**
	 * Pass exactly one String.
	 */
	protected String doInBackground(String... params) {
		if (params.length != 1) {
			throw new IllegalArgumentException("Must pass exactly one string to doInBackground");
		}

		// Set the cancel indicator in the evaluator. This is a Scheme Droid
		// enhancement of JScheme.
		js.getEvaluator().setCancelIndicator(this);
		
		// Execute the code, being very cautious to catch any and all Throwables
		// that can occur.
		String code = params[0];
		String result;

		try {
			Object ret = js.eval(code);
			if (ret == jsint.Primitive.DO_NOT_DISPLAY) {
				result = "";
			} else {
				result = jsint.U.stringify(ret);
			}
		} catch (final jscheme.SchemeException e) {
			result = e.getMessage();
		} catch (final jsint.BacktraceException e) {
			result = e.getMessage();
		} catch (final Exception e) {
			result = "Generic Error: " + e.toString();
		} catch (final Error e) {
			result = "Critical Error: " + e.toString();
		} catch (final Throwable e) {
			result = "Unknown Error: " + e.toString();
		}

		return result;
	}

	protected void onPostExecute(String result) {
		fragment.clearEvalTask();
		console.append(result);
		runningIndicator.setVisibility(View.INVISIBLE);
		entry.setEnabled(true);
	}
}

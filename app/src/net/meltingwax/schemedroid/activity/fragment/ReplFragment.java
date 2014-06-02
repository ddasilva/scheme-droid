package net.meltingwax.schemedroid.activity.fragment;

import java.io.BufferedOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.LinkedList;

import jscheme.JScheme;
import net.meltingwax.schemedroid.R;
import net.meltingwax.schemedroid.activity.SchemeResources;
import net.meltingwax.schemedroid.ui.EntryHighlighter;
import net.meltingwax.schemedroid.util.BaseAsyncTaskLoader;
import net.meltingwax.schemedroid.util.EvalAsyncTask;
import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
import android.os.Handler;
import android.support.v4.app.Fragment;
import android.support.v4.app.LoaderManager.LoaderCallbacks;
import android.support.v4.content.Loader;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.TextView.OnEditorActionListener;
import android.widget.Toast;

/**
 * Read Eval Print Loop activity.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 * @author Olexandr Tereshchuk - <a href="http://stanfy.com.ua">Stanfy LLC</a>
 */
public class ReplFragment extends Fragment implements LoaderCallbacks<String> {

	/** Logging tag. */
	public static final String TAG = "ReplFragment";

	/** Arguments. */
	public static final String ARG_FILE = "scheme_script_file";

	/** Loader ID. */
	private static final int LOADER_SCRIPT = 1, LOADER_INIT = 2;

	/** Init file. */
	private static final String INIT_FILE = "jscheme.init";

	/** JScheme core. */
	private JScheme js;
	/** Console output. */
	private TextView console;
	/** Console input. */
	private EditText entry;

	/** Current evaluation async task */
	private EvalAsyncTask currentEvalTask;

	/** Output handler */
	private Handler outputHandler;
	
	/** File was loaded. */
	private boolean loadedFile = false, inited = false;

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setHasOptionsMenu(true);
		setRetainInstance(true);

		js = new JScheme();
		currentEvalTask = null;
	}

	@Override
	public View onCreateView(final LayoutInflater inflater,
			final ViewGroup container, final Bundle savedInstanceState) {
		return inflater.inflate(R.layout.repl, container, false);
	}

	@Override
	public void onViewCreated(final View view, final Bundle savedInstanceState) {
		super.onViewCreated(view, savedInstanceState);
		
		/*
		 * Console Configuration
 		 * - Set font to MonoSpace
		 * - Restore text from old saved instance, it one exists
		 * - Tie the JScheme output to the console
		 */
		TextView oldConsole = console;
		console = (TextView) view.findViewById(R.id.console);
		console.setTypeface(Typeface.MONOSPACE);

		if (oldConsole != null) {
			console.setText(oldConsole.getText());
		}

		outputHandler = new Handler();
		js.getEvaluator().setOutput(
			/* this is ridiculous and needs to be rewritten */
			new PrintWriter(new BufferedOutputStream(new OutputStream() {
				public void write(final int oneByte) throws IOException {
					outputHandler.post(new Runnable() {
						public void run() {
							char charArray[] = Character.toChars(oneByte);
							console.append(new String(charArray));
						}
					});
				}
		})));

		/*
		 * Entry (input) Configuration
		 * - Set font to MonoSpace
		 * - Request focus
		 * - Restore text from old saved instance, if one exists
		 * - Attach new EntryHighlighter instance to response to changes
		 * - Attach anonymous listeners to respond to enter press
		 */
		EditText oldEntry = entry;
		entry = (EditText) view.findViewById(R.id.code_input);
		entry.setTypeface(Typeface.MONOSPACE);
		entry.requestFocus();

		if (oldEntry != null) {
			entry.setText(oldEntry.getText());
		}

		entry.addTextChangedListener(new EntryHighlighter(entry));

		entry.setOnEditorActionListener(new OnEditorActionListener() {
			@Override
			public boolean onEditorAction(final TextView v, final int actionId,
					final KeyEvent event) {
				// Consume all events, acting only on KeyEvent.ACTION_DOWN.
				// If we do not consume KeyEvent.ACTION_UP as well, the
				// code input does not retain focus after enter is pushed on
				// a physical keyboard.
				if (EditorInfo.IME_NULL == actionId && event != null) {
					if (event.getAction() == KeyEvent.ACTION_DOWN) {
						processEntry();
					}
					return true;
				}

				return false;
			}
		});
		
		/*
		 * Eval Button Configuration
		 * - Attach anonymous listener to respond to click
		 */
		view.findViewById(R.id.button_eval).setOnClickListener(
				new View.OnClickListener() {
					@Override
					public void onClick(final View v) {
						processEntry();
					}
				});
	}

	@Override
	public void onActivityCreated(final Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		if (!inited) {
			init();
		}
		if (!loadedFile && getArguments() != null
				&& getArguments().containsKey(ARG_FILE)) {
			getLoaderManager().initLoader(LOADER_SCRIPT, getArguments(), this);
		}
	}

	@Override
	public void onCreateOptionsMenu(final Menu menu, final MenuInflater inflater) {
		super.onCreateOptionsMenu(menu, inflater);
		inflater.inflate(R.menu.menu_repl, menu);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		if (item.getItemId() == R.id.menu_cancel) {
			cancel();
			return true;
		}

		if (item.getItemId() == R.id.menu_reset) {
			reset();
			return true;
		}

		if (item.getItemId() == R.id.menu_resources) {
			startActivity(new Intent(getActivity(), SchemeResources.class));
			return true;
		}

		return false;
	}

	private void init() {
		final Bundle args = new Bundle(1);
		args.putString(ARG_FILE, INIT_FILE);
		getLoaderManager().initLoader(LOADER_INIT, args, this);
	}

	public void reset() {
		js = new JScheme();
		console.setText("");
		entry.setText("");
		inited = false;
		init();
	}

	private void cancel() {
		if (currentEvalTask != null) {
			if (!currentEvalTask.cancel(true)) {
				Log.e(TAG, "Could not cancel the eval task");
			}
		}
	}

	/**
	 * Processes the code in the entry EditText and updates the UI.
	 */
	private void processEntry() {
		final String code = entry.getText().toString().trim();

		if (code.length() > 0) {
			console.append("\n> " + code + "\n");
			currentEvalTask = new EvalAsyncTask(this);
			currentEvalTask.execute(code);
			entry.setText("");
			entry.setEnabled(false);
		} else {
			Toast.makeText(getActivity(), R.string.error_code_empty,
					Toast.LENGTH_SHORT).show();
		}
	}

	public void clearEvalTask() {
		currentEvalTask = null;
	}
	
	public TextView getConsole() {
		return console;
	}
	
	public EditText getEntry() {
		return entry;
	}
	
	public JScheme getJScheme() {
		return js;
	}

	public EvalAsyncTask getCurrentEvalTask() {
		return currentEvalTask;
	}

	@Override
	public Loader<String> onCreateLoader(final int loaderId, final Bundle args) {
		entry.setEnabled(false);
		entry.setHint(R.string.input_code_loading_hint);
		return new FileLoader(this, args.getString(ARG_FILE));
	}

	@Override
	public void onLoadFinished(final Loader<String> loader, final String data) {
		if (loader.getId() == LOADER_INIT) {
			inited = true;
		} else {
			loadedFile = true;
		}
		console.append("\n> (load '" + ((FileLoader) loader).getPath() + "')\n"
				+ data);
		getLoaderManager().destroyLoader(loader.getId());
		if (getLoaderManager().getLoader(LOADER_INIT) == null
				&& getLoaderManager().getLoader(LOADER_SCRIPT) == null) {
			entry.setEnabled(true);
			entry.setHint(R.string.input_code_hint);
		}
	}

	@Override
	public void onLoaderReset(final Loader<String> loader) { /* nothing */
	}

	/**
	 * Script file loader.
	 * 
	 * @author Olexandr Tereshchuk - <a href="http://stanfy.com.ua">Stanfy
	 *         LLC</a>
	 */
	private static class FileLoader extends BaseAsyncTaskLoader<String> {

		/** Fragment instance. */
		private final ReplFragment fragment;
		/** File path. */
		private final String path;

		public FileLoader(final ReplFragment fragment, final String path) {
			super(fragment.getActivity());
			this.fragment = fragment;
			this.path = path;
		}

		@Override
		public String loadInBackground() {
			Reader reader = null;
			try {
				if (INIT_FILE.equals(path)) {
					reader = new InputStreamReader(fragment.getResources()
							.openRawResource(R.raw.jscheme),
							Charset.defaultCharset());
				} else {
					reader = new FileReader(path);
				}
				if (jsint.U.to_bool(fragment.js.load(reader))) {
					return fragment.getString(R.string.file_load_success);
				}
				return fragment.getString(R.string.error_file_load);
			} catch (final Throwable t) {
				return t.getMessage();
			} finally {
				if (reader != null) {
					try {
						reader.close();
					} catch (final IOException t) {
						Log.e(TAG, "Can't close stream", t);
					}
				}
			}
		}

		public String getPath() {
			return path;
		}
	}

	
}

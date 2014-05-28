package net.meltingwax.schemedroid.activity.fragment;

import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.LinkedList;

import jscheme.JScheme;
import net.meltingwax.schemedroid.R;
import net.meltingwax.schemedroid.activity.SchemeResources;
import net.meltingwax.schemedroid.util.BaseAsyncTaskLoader;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.Typeface;
import android.os.Bundle;
import android.os.Handler;
import android.support.v4.app.Fragment;
import android.support.v4.app.LoaderManager.LoaderCallbacks;
import android.support.v4.content.Loader;
import android.text.Editable;
import android.text.Spannable;
import android.text.TextWatcher;
import android.text.style.BackgroundColorSpan;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
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

	/** Time to highlight recently closed parenthesis region (ms) */
	private static final long HIGHLIGHT_REGION_TIME = 500;

	/** Color to highlight recently closed parenthesis region */
	private static final String HIGHLIGHT_REGION_COLOR = "#2461bd";

	/** JScheme core. */
	private JScheme js;
	/** Console output. */
	private TextView console;
	/** Console input. */
	private EditText entry;

	/** File was loaded. */
	private boolean loadedFile = false, inited = false;

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setHasOptionsMenu(true);
		setRetainInstance(true);

		js = new JScheme();
	}

	@Override
	public View onCreateView(final LayoutInflater inflater,
			final ViewGroup container, final Bundle savedInstanceState) {
		return inflater.inflate(R.layout.repl, container, false);
	}

	@Override
	public void onViewCreated(final View view, final Bundle savedInstanceState) {
		super.onViewCreated(view, savedInstanceState);
		final TextView oldConsole = console;
		console = (TextView) view.findViewById(R.id.console);
		if (oldConsole != null) {
			console.setText(oldConsole.getText());
		}
		console.setTypeface(Typeface.MONOSPACE);

		final EditText oldEntry = entry;
		entry = (EditText) view.findViewById(R.id.code_input);
		if (oldEntry != null) {
			entry.setText(oldEntry.getText());
		}

		entry.addTextChangedListener(new TextWatcher() {
			@Override
			public void onTextChanged(CharSequence s, int start, int before,
					int count) {
				CharSequence text = entry.getText();
				int textLength = text.length();
				int cursorPos = start + count - 1;

				if (textLength > 0
						&& entry.getText().charAt(cursorPos) == ')') {

					/* search for last unbalanced left parenthesis */
					LinkedList<Integer> openParens = new LinkedList<Integer>();
					boolean inQuotes = false;
					boolean escaped = false;

					for (int i = 0; i < cursorPos; i++) {
						char currentChar = text.charAt(i);

						if (escaped) {
							escaped = false;
						} else if (currentChar == '"' && ! escaped) {
							inQuotes = ! inQuotes;
						} else if (currentChar == '(' && ! inQuotes) {
							openParens.addLast(Integer.valueOf(i));
						} else if (currentChar == ')' && ! inQuotes && ! openParens.isEmpty()) {
							openParens.removeLast();
						}
					}

					/* highlight parenthesis region just closed */
					if (openParens.size() > 0) {
						final int idxLastOpenParen = openParens.getLast().intValue();
						final BackgroundColorSpan span = new BackgroundColorSpan(
								Color.parseColor(HIGHLIGHT_REGION_COLOR));
						final Handler handler = new Handler();

						entry.getText().setSpan(span, idxLastOpenParen, cursorPos + 1,
								Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

						handler.postDelayed(new Runnable() {
							@Override
							public void run() {
								entry.getText().removeSpan(span);
							}
						}, HIGHLIGHT_REGION_TIME);
					}
				}
			}

			@Override
			public void afterTextChanged(Editable s) {
			}

			@Override
			public void beforeTextChanged(CharSequence s, int start,
					int count, int after) {
			}
		});

		entry.setOnEditorActionListener(new OnEditorActionListener() {
			@Override
			public boolean onEditorAction(final TextView v, final int actionId,
					final KeyEvent event) {
				if (EditorInfo.IME_ACTION_DONE == actionId || (event != null
						&& event.getAction() == KeyEvent.ACTION_DOWN
						&& event.getKeyCode() == KeyEvent.KEYCODE_ENTER)) {
					processEntry();
					return true;
				}
				return false;
			}
		});

		entry.setTypeface(Typeface.MONOSPACE);

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

	private void reset() {
		js = new JScheme();
		console.setText("");
		entry.setText("");
		inited = false;
		init();
	}

	/**
	 * Processes the code in the entry EditText and updates the UI.
	 */
	private void processEntry() {
		final String code = entry.getText().toString().trim();

		if (code.length() > 0) {
			String resp;

			try {
				resp = jsint.U.stringify(js.eval(code));
			} catch (final jscheme.SchemeException e) {
				resp = e.getMessage();
			} catch (final jsint.BacktraceException e) {
				resp = e.getMessage();
			} catch (final Exception e) {
				resp = "Generic Error: " + e.toString();
			}

			console.append("\n> " + code + "\n" + resp);

			entry.setText("");
		} else {
			Toast.makeText(getActivity(), R.string.error_code_empty,
					Toast.LENGTH_SHORT).show();
		}
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

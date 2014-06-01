package net.meltingwax.schemedroid.ui;

import java.util.LinkedList;

import android.graphics.Color;
import android.os.Handler;
import android.text.Editable;
import android.text.Spannable;
import android.text.TextWatcher;
import android.text.style.BackgroundColorSpan;
import android.widget.EditText;

/**
 * Provides a TextWatcher implementation over a EditText to implement
 * highlighting of parenthesis regions. Used in the entry box of the REPL.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 * 
 */
public class EntryHighlighter implements TextWatcher {

	/** The EditText we are watching for changes */
	private final EditText entry;

	/** Time to highlight recently closed parenthesis region (ms) */
	private static final long HIGHLIGHT_REGION_TIME = 500;

	/** Color to highlight recently closed parenthesis region */
	private static final String HIGHLIGHT_REGION_COLOR = "#2461bd";

	public EntryHighlighter(EditText entry) {
		this.entry = entry;
	}

	@Override
	public void onTextChanged(CharSequence s, int start, int before, int count) {
		final CharSequence text = entry.getText();
		final int textLength = text.length();
		final int cursorPos = start + count - 1;

		// Acting conditions
		final boolean nonEmpty = (textLength > 0);
		final boolean addingNewChar = (before == 0);
		final boolean cursorAtRightParen = (nonEmpty && text.charAt(cursorPos) == ')');

		if (nonEmpty && addingNewChar && cursorAtRightParen) {
			// Search for last unbalanced left parenthesis
			final LinkedList<Integer> openParens = new LinkedList<Integer>();
			boolean inQuotes = false;
			boolean escaped = false;

			for (int i = 0; i < cursorPos; i++) {
				final char currentChar = text.charAt(i);

				if (escaped) {
					escaped = false;
				} else if (currentChar == '"' && !escaped) {
					inQuotes = !inQuotes;
				} else if (currentChar == '(' && !inQuotes) {
					openParens.addLast(Integer.valueOf(i));
				} else if (currentChar == ')' && !inQuotes && !openParens.isEmpty()) {
					openParens.removeLast();
				}
			}

			// Create a span to highlight the recently closed parenthesis region
			if (openParens.size() > 0) {
				int idxLastOpenParen = openParens.getLast().intValue();

				final BackgroundColorSpan span = new BackgroundColorSpan(
						Color.parseColor(HIGHLIGHT_REGION_COLOR));

				entry.getText().setSpan(span, idxLastOpenParen, cursorPos + 1,
						Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

				Handler handler = new Handler();
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
		// Take no action
	}

	@Override
	public void beforeTextChanged(CharSequence s, int start, int count,
			int after) {
		// Take no action
	}
}
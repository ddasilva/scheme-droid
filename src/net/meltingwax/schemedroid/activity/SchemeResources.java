package net.meltingwax.schemedroid.activity;

import net.meltingwax.schemedroid.R;
import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;

/**
 * Resources.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 * @author Olexandr Tereshchuk - <a href="http://stanfy.com.ua">Stanfy LLC</a>
 */
public class SchemeResources extends Activity implements View.OnClickListener {

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		final ScrollView container = new ScrollView(this);
		final LinearLayout parent = new LinearLayout(this);
		parent.setOrientation(LinearLayout.VERTICAL);

		container.addView(parent);
		container.setFillViewport(true);

		final TextView linksText = new TextView(this);
		linksText.setText("\nLinks (open in browser)\n");
		linksText.setGravity(Gravity.CENTER);
		linksText.setTextSize(18);
		parent.addView(linksText);

		addLinkButtonToLinearLayout(parent,
				"Calling Java Classes using Scheme Droid",
				"http://jscheme.sourceforge.net/jscheme/doc/javaprimitives.html");
		addLinkButtonToLinearLayout(parent, "SICP Table of Contents",
				"http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html");
		addLinkButtonToLinearLayout(parent,
				"R5RS Scheme Standard Table of Contents",
				"http://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-2.html");
		addLinkButtonToLinearLayout(parent, "Reference Manual for Jscheme",
				"http://jscheme.sourceforge.net/jscheme/doc/refman.html");
		addLinkButtonToLinearLayout(parent, "Schemers.org",
				"http://schemers.org/");

		setContentView(container);
		setTitle(R.string.title_resources);
	}

	private void addLinkButtonToLinearLayout(final LinearLayout parent,
			final String title, final String url) {
		final Button button = new Button(this);
		button.setTag(url);
		button.setText(title);
		button.setOnClickListener(this);
		parent.addView(button);
	}

	@Override
	public void onClick(final View v) {
		startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(v.getTag()
				.toString())));
	}
}

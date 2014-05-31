package net.meltingwax.schemedroid;

import net.meltingwax.schemedroid.activity.fragment.ReplFragment;
import android.os.Bundle;
import android.support.v4.app.FragmentActivity;

/**
 * The main Scheme Droid home screen activity.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 * @author Olexandr Tereshchuk - <a href="http://stanfy.com.ua">Stanfy LLC</a>
 */
public class SchemeDroid extends FragmentActivity {

	private ReplFragment replFragment;

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setTitle(R.string.title_repl);

		if (getSupportFragmentManager().findFragmentByTag(ReplFragment.TAG) == null) {
			final Bundle args = new Bundle(1);
			if (getIntent().getData() != null) {
				args.putString(ReplFragment.ARG_FILE, getIntent().getData()
						.getPath());
			}
			replFragment = new ReplFragment();
			replFragment.setArguments(args);
			getSupportFragmentManager().beginTransaction()
					.replace(android.R.id.content, replFragment, ReplFragment.TAG)
					.commit();
		}
	}

	public ReplFragment getReplFragment() {
		return replFragment;
	}
}
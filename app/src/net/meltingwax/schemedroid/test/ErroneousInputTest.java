package net.meltingwax.schemedroid.test;

import net.meltingwax.schemedroid.SchemeDroid;
import android.app.Activity;
import android.test.ActivityInstrumentationTestCase2;
import android.test.UiThreadTest;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

/**
 * Sends inputs incorrect code and ensures the activity does not crash
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 */
public class ErroneousInputTest extends 
		ActivityInstrumentationTestCase2<SchemeDroid>{

	private SchemeDroid activity;
	private EditText entry;
	private Button evalButton;

	public ErroneousInputTest() {
		super(SchemeDroid.class);
	}

	protected void setUp() throws Exception {
		super.setUp();
		setActivityInitialTouchMode(false);

		activity = getActivity();

		entry = (EditText) activity.findViewById(
				net.meltingwax.schemedroid.R.id.code_input);

		evalButton = (Button) activity.findViewById(
				net.meltingwax.schemedroid.R.id.button_eval);
	}

	private void doTest(String ... inputs) {
		activity.getReplFragment().reset();

		for (String input : inputs) {
			entry.setText(input);
			evalButton.callOnClick();
		}
	}

	@UiThreadTest
	public void testCallUndefinedFunction() {
		doTest("(foobar)");
	}

	@UiThreadTest
	public void testCallWithUnbalancedParenthesis() {
		doTest("(foo", "bar)");
	}

	@UiThreadTest
	public void testReferenceUndefinedVariable() {
		doTest("foobar", "foo", "bar", "foo-bar");
	}

	@UiThreadTest
	public void testCdrEmptyList() {
		doTest("(cdr '())");
	}
}

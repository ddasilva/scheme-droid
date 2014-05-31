package net.meltingwax.schemedroid.test;

import net.meltingwax.schemedroid.SchemeDroid;
import android.app.Activity;
import android.test.ActivityInstrumentationTestCase2;
import android.test.UiThreadTest;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

public class SchemeDroidTest extends
		ActivityInstrumentationTestCase2<SchemeDroid> {

	private Activity activity;
	private TextView console;
	private EditText entry;
	private Button evalButton;
	
	public SchemeDroidTest() {
		super(SchemeDroid.class);
	}

	protected void setUp() throws Exception {
		super.setUp();
		setActivityInitialTouchMode(false);

		activity = getActivity();
		
		console = (TextView) activity.findViewById(
				net.meltingwax.schemedroid.R.id.console);
		
		entry = (EditText) activity.findViewById(
				net.meltingwax.schemedroid.R.id.code_input);

		evalButton = (Button) activity.findViewById(
				net.meltingwax.schemedroid.R.id.button_eval);
	}
	
	@UiThreadTest
	public void testErronousInput() {
		entry.requestFocus();
		entry.setText("(a");
		evalButton.callOnClick();
		assertTrue("Test", false);
	}
}

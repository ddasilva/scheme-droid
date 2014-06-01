package net.meltingwax.schemedroid.test.inputs;

import net.meltingwax.schemedroid.SchemeDroid;
import android.test.ActivityInstrumentationTestCase2;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

public class InputSetTestCase extends
	ActivityInstrumentationTestCase2<SchemeDroid>{

	private SchemeDroid activity;
	private TextView console;
	private EditText entry;
	private Button evalButton;
	
	public InputSetTestCase() {
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

	protected void sendInput(String ... inputs) {
		activity.getReplFragment().reset();

		for (final String input : inputs) {
			entry.setText(input);
			evalButton.callOnClick();
		}
	}

	protected void assertConsoleContains(String text) {
		final String message =
				"Console does not contain: " + text + "\n"
				+ "Consoles contents are: " + console.getText();
		final boolean assertion = console.getText().toString().contains(text);

		assertTrue(message, assertion);
	}
}

package net.meltingwax.schemedroid.test.inputs;

import android.test.UiThreadTest;

/**
 * Sends inputs incorrect code and ensures the activity does not crash.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 */
public class ErroneousInputTest extends InputSetTestCase {

	@UiThreadTest
	public void testCallUndefinedFunction() {
		sendInput("(foobar)");
	}

	@UiThreadTest
	public void testCallWithUnbalancedParenthesis() {
		sendInput("(foo", "bar)");
	}

	@UiThreadTest
	public void testReferenceUndefinedVariable() {
		sendInput("foobar", "foo", "bar", "foo-bar");
	}

	@UiThreadTest
	public void testCdrEmptyList() {
		sendInput("(cdr '())");
	}
}

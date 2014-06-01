package net.meltingwax.schemedroid.test;

import android.test.UiThreadTest;

/**
 * Sends inputs incorrect code and ensures the activity does not crash.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 */
public class ErroneousInputTest extends InputSetTestCase {

	@UiThreadTest
	public void testCallUndefinedFunction() {
		doInputTest("(foobar)");
	}

	@UiThreadTest
	public void testCallWithUnbalancedParenthesis() {
		doInputTest("(foo", "bar)");
	}

	@UiThreadTest
	public void testReferenceUndefinedVariable() {
		doInputTest("foobar", "foo", "bar", "foo-bar");
	}

	@UiThreadTest
	public void testCdrEmptyList() {
		doInputTest("(cdr '())");
	}
}

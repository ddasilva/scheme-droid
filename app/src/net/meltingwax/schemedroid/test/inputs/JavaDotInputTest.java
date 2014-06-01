package net.meltingwax.schemedroid.test.inputs;

import android.test.UiThreadTest;

public class JavaDotInputTest extends InputSetTestCase {

	@UiThreadTest
	public void testMathClass() {
		sendInput("(Math.sin 1.0)");
		assertConsoleContains(".84");

		sendInput("(Math.cos 1.0)");
		assertConsoleContains(".54");

		sendInput("(Math.tan 1.0)");
		assertConsoleContains("1.55");
	}
}

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

	@UiThreadTest
	public void testImportLinkedList() {
		sendInput("(import \"java.util.LinkedList\")");
		assertConsoleContains("#t");
	}

	@UiThreadTest
	public void testConstructLinkedList() {
		sendInput(
				"(import \"java.util.LinkedList\")",
				"(LinkedList.)");
		assertConsoleContains("[]");
	}

	@UiThreadTest
	public void testAddToLinkedList() {
		sendInput(
				"(import \"java.util.LinkedList\")",
				"(define element \"foobar\")",
				"(define my-ll (LinkedList.))",
				"(.add my-ll element)",
				"(define ll-size (.size my-ll))",
				"(display (string-append \"{ll-size:\" ll-size \"}\"))",
				"(define ll-head (.get my-ll 0))",
				"(define are-eq (eq? ll-head element))",
				"(display (string-append \"{are-eq:\" are-eq \"}\"))");
		assertConsoleContains("{ll-size:1}");
		assertConsoleContains("{are-eq:#t}");
	}
}

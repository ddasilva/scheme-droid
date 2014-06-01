package net.meltingwax.schemedroid.test.inputs;

import android.test.UiThreadTest;

/**
 * Sends inputs incorrect code and ensures the activity does not crash.
 * 
 * @author daniel@meltingwax.net (Daniel da Silva)
 */
public class ValidInputTest extends InputSetTestCase {

	@UiThreadTest
	public void testNumberLiteral() {
		for (int i = -100; i <= 100; i++) {
			sendInput(i + "");
		}
	}

	@UiThreadTest
	public void testDefineFunction() {
		sendInput("(define (f x) (+ x 1))");
		sendInput("(define f (lambda (x) (+ x 1)))");
	}

	@UiThreadTest
	public void testApplyFunction() {
		sendInput("(define (f x) (+ x 1))", "(f 100)");
		assertConsoleContains("101");

		sendInput("(define (f ls) "
					+ "(if (null? ls) '() "
					+ "(cons (* 2 (car ls)) (f (cdr ls)))))",
					"(f '(300 400 500))");
		assertConsoleContains("600");
		assertConsoleContains("800");
		assertConsoleContains("1000");
	}

	@UiThreadTest
	public void testMapBuiltin() {
		sendInput("(map (lambda (x) (- x 2)) '(0 5 99))");
		assertConsoleContains("-2");
		assertConsoleContains("3");
		assertConsoleContains("97");
	}

	@UiThreadTest
	public void testConsNumberAndNumber() {
		sendInput("(cons 3 4)");
		assertConsoleContains("(3 . 4)");
	}

	@UiThreadTest
	public void testConsNumberAndEmptyList() {
		sendInput("(cons 3 '())");
		assertConsoleContains("(3)");
	}
}

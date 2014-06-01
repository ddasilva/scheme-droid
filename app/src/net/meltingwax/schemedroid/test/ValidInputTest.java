package net.meltingwax.schemedroid.test;

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
			doInputTest(i + "");
		}
	}
	
	@UiThreadTest
	public void testDefineFunction() {
		doInputTest("(define (f x) (+ x 1))");
		doInputTest("(define f (lambda (x) (+ x 1)))");
	}
	
	@UiThreadTest
	public void testApplyFunction() {
		doInputTest("(define (f x) (+ x 1))", "(f 100)");
		assertConsoleContains("101");

		doInputTest("(define (f ls) "
					+ "(if (null? ls) '() "
					+ "(cons (* 2 (car ls)) (f (cdr ls)))))",
					"(f '(300 400 500))");
		assertConsoleContains("600");
		assertConsoleContains("800");
		assertConsoleContains("1000");
	}
}

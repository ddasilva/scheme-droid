
import jscheme.JScheme;

public class Test {
	public static void main(String[] args) {
		JScheme js = new JScheme();
		js.eval("(+ 1 2)");
		js.eval("(asdf");
	}
}

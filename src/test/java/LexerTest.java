import com.en_circle.slt.plugin.lisp.LispLexerAdapter;
import com.intellij.psi.tree.IElementType;

public class LexerTest {

    public static void main(String[] args) {
        LispLexerAdapter adapter = new LispLexerAdapter();
        adapter.start("*| block\n" +
                "comment |*\n" +
                "\n" +
                "(defun foo (bar &args baz)\n" +
                "    (setf qux (* 2 baz))\n" +
                "    (progn ;hello\n" +
                "        (let ((a 5)\n" +
                "              (b qux))\n" +
                "          (if bar\n" +
                "            (format T \"Hello %s\" b)\n" +
                "            (format T \"World %s\" a)))))");
        while (true) {
            IElementType token = adapter.getTokenType();
            if (token == null)
                return;

            System.out.println("TokenType: " + token.getDebugName() + ": " + adapter.getTokenText());
            adapter.advance();
        }
    }

}

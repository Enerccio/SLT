import com.en_circle.slt.plugin.lisp.LispLexerAdapter;
import com.intellij.psi.tree.IElementType;

public class LexerTest {

    public static void main(String[] args) {
        LispLexerAdapter adapter = new LispLexerAdapter();
        adapter.start("*|\n" +
                "wot:\n" +
                "- Author: pvan\n" +
                "- Date: 2023-01-06\n" +
                "|*\n" +
                "\n" +
                ":ar\n" +
                "*|\n" +
                "(defmacro loop-three-times (form)\n" +
                "    `(progn ,form ,form ,form))\n" +
                "\n" +
                "(loop-three-times (print \"Anca\"))\n" +
                "(print \"Mara\")\n" +
                "(defun bobz (foo bar &rest args)\n" +
                "    (bobz (+ foo 1) bar args :vegana))\n" +
                "|*");
        while (true) {
            IElementType token = adapter.getTokenType();
            if (token == null)
                return;

            System.out.println("TokenType: " + token.getDebugName() + ": " + adapter.getTokenText());
            adapter.advance();
        }
    }

}

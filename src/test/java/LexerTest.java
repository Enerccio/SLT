import com.en_circle.slt.plugin.lisp.LispLexerAdapter;
import com.en_circle.slt.templates.CodeHighlightTemplate;
import com.intellij.psi.tree.IElementType;

public class LexerTest {

    public static void main(String[] args) {
        LispLexerAdapter adapter = new LispLexerAdapter();
        adapter.start(new CodeHighlightTemplate().render());
        while (true) {
            IElementType token = adapter.getTokenType();
            if (token == null)
                return;

            System.out.println("TokenType: " + token.getDebugName() + ": " + adapter.getTokenText());
            adapter.advance();
        }
    }

}

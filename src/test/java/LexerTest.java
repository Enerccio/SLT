import com.en_circle.slt.plugin.lisp.LispLexerAdapter;
import com.intellij.psi.tree.IElementType;

public class LexerTest {

    public static void main(String[] args) {
        LispLexerAdapter adapter = new LispLexerAdapter();
        adapter.start("(:new-features\n" +
                "    (:swank :quicklisp :asdf3.3 :asdf3.2 :asdf3.1 :asdf3 :asdf2 :asdf :os-unix :non-base-chars-exist-p\n" +
                "    :asdf-unicode :arena-allocator :x86-64 :gencgc :64-bit :ansi-cl :common-lisp\n" +
                "    :elf :ieee-floating-point :linux :little-endian :package-local-nicknames\n" +
                "    :sb-core-compression :sb-ldb :sb-package-locks :sb-thread :sb-unicode :sbcl :unix))");
        while (true) {
            IElementType token = adapter.getTokenType();
            if (token == null)
                return;

            System.out.println("TokenType: " + token.getDebugName() + ": " + adapter.getTokenText());
            adapter.advance();
        }
    }

}

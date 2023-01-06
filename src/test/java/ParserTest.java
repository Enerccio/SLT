import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltCommonLispParserDefinition;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.lisp.psi.LispCoreProjectEnvironment;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;

public class ParserTest {

    public static void main(String[] args) {
        String data = "(:new-features\n" +
                "    (:swank :quicklisp :asdf3.3 :asdf3.2 :asdf3.1 :asdf3 :asdf2 :asdf :os-unix :non-base-chars-exist-p\n" +
                "    :asdf-unicode :arena-allocator :x86-64 :gencgc :64-bit :ansi-cl :common-lisp\n" +
                "    :elf :ieee-floating-point :linux :little-endian :package-local-nicknames\n" +
                "    :sb-core-compression :sb-ldb :sb-package-locks :sb-thread :sb-unicode :sbcl :unix))";

        LispCoreProjectEnvironment projectEnvironment = new LispCoreProjectEnvironment();
        projectEnvironment.getEnvironment()
                .registerParserDefinition(SltCommonLispLanguage.INSTANCE, new SltCommonLispParserDefinition());
        PsiFileFactory factory = PsiFileFactory.getInstance(projectEnvironment.getProject());
        PsiFile source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        System.out.println(LispUtils.convertAst(source));
    }

}

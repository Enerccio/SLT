import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltCommonLispParserDefinition;
import com.en_circle.slt.plugin.lisp.psi.LispCoreProjectEnvironment;
import com.en_circle.slt.plugin.lisp.psi.LispVisitor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import org.jetbrains.annotations.NotNull;

public class ParserTest {

    public static void main(String[] args) {
        String data = """
                (defun
                    buble
                    (a b c) 0
                """;

        LispCoreProjectEnvironment projectEnvironment = new LispCoreProjectEnvironment();
        projectEnvironment.getEnvironment()
                .registerParserDefinition(SltCommonLispLanguage.INSTANCE, new SltCommonLispParserDefinition());
        PsiFileFactory factory = PsiFileFactory.getInstance(projectEnvironment.getProject());
        PsiFile source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        source.accept(new LispVisitor() {
            @Override
            public void visitElement(@NotNull PsiElement element) {
                System.out.println(element);
                element.acceptChildren(this);
            }
        });
    }
}

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltCommonLispParserDefinition;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.lisp.psi.LispCoreProjectEnvironment;
import com.en_circle.slt.plugin.swank.SwankClient;
import com.en_circle.slt.plugin.swank.SwankClient.SwankReply;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;

import java.util.List;

public class SwankTest {

    public static void main(String[] args) throws Exception {
        try {
            try (SwankClient client = new SwankClient("127.0.0.1", 12345, new SwankReply() {
                @Override
                public void onSwankMessage(SwankPacket packet) {
                    LispCoreProjectEnvironment projectEnvironment = new LispCoreProjectEnvironment();
                    projectEnvironment.getEnvironment()
                            .registerParserDefinition(SltCommonLispLanguage.INSTANCE, new SltCommonLispParserDefinition());
                    PsiFileFactory factory = PsiFileFactory.getInstance(projectEnvironment.getProject());
                    PsiFile source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, packet.getSentData());
                    List<LispElement> elements = LispUtils.convertAst(source);
                    for (LispElement e : elements) {
                        System.out.println(e.toPrettyString() + "\n");
                    }
                }

                @Override
                public void onReadError(Exception e) {
                    e.printStackTrace();
                }
            })) {

//                client.swankSend(SlimePacket.swankInteractiveEval("(+ 1 1)", "cl-user", 1));
//                client.swankSend(new SlimePacket("(:emacs-rex (swank:describe-definition-for-emacs \"XAXA\" :function) \"cl-user\" T 2)"));
                  client.swankSend(new SwankPacket("(:emacs-rex (swank:interactive-eval \"(error \"1\")\") \"cl-user\" T 1)"));
                // - finding symbols!
//                client.swankSend(new SlimePacket("(:emacs-rex (swank:apropos-list-for-emacs \"defun\") \"cl-user\" T 2)"));
//                client.swankSend(new SlimePacket("(:emacs-rex (swank:apropos-list-for-emacs \"defun\") \"cl-user\" T 2)"));
//                client.swankSend(SlimePacket.rpcReturnOk("(+ 1 2)", 2));
//                client.swankSend(SlimePacket.rpcWriteString("(+ 4 5)"));
//                client.swankSend(SlimePacket.swankInteractiveEval("(macro-function 'defun)", "cl-user", 1));

                Thread.sleep(10000);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}

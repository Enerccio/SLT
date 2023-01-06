package com.en_circle.slt.plugin.swank;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltCommonLispParserDefinition;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.lisp.psi.LispCoreProjectEnvironment;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;

import java.util.concurrent.atomic.AtomicLong;

public class SlimeListener implements SwankClient.SwankReply {

    private static final AtomicLong rpcIdentifier = new AtomicLong();


    @Override
    public void onSwankMessage(SlimePacket packet) {
        String data = packet.getSentData();

        LispCoreProjectEnvironment projectEnvironment = new LispCoreProjectEnvironment();
        projectEnvironment.getEnvironment()
                .registerParserDefinition(SltCommonLispLanguage.INSTANCE, new SltCommonLispParserDefinition());
        PsiFileFactory factory = PsiFileFactory.getInstance(projectEnvironment.getProject());
        PsiFile source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        System.out.println(LispUtils.convertAst(source));
    }

}

package com.en_circle.slt.plugin.swank;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltCommonLispParserDefinition;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.lisp.psi.LispCoreProjectEnvironment;
import com.en_circle.slt.plugin.swank.requests.SltEval;
import com.en_circle.slt.plugin.swank.requests.SwankEvalAndGrab;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;

import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SlimeListener implements SwankClient.SwankReply {

    private static BigInteger rpcIdentifier = new BigInteger("0");

    public static synchronized BigInteger nextRpc() {
        BigInteger next = rpcIdentifier.add(BigInteger.ONE);
        rpcIdentifier = next;
        return next;
    }

    private final Project project;
    private final boolean fromUi;
    private final Map<BigInteger, SlimeRequest> requests = Collections.synchronizedMap(new HashMap<>());
    private final RequestResponseLogger logger;

    public SlimeListener(Project project, boolean fromUi, RequestResponseLogger logger) {
        this.project = project;
        this.fromUi = fromUi;
        this.logger = logger;
    }

    public void call(SlimeRequest request, SwankClient client) {
        BigInteger requestId = nextRpc();
        requests.put(requestId, request);
        SwankPacket packet = request.createPacket(requestId);
        if (logger != null) {
            logger.logRequest(packet.getSentData());
        }
        client.swankSend(packet);
    }

    @Override
    public void onSwankMessage(SwankPacket packet) {
        String data = packet.getSentData();

        if (fromUi) {
            ApplicationManager.getApplication().invokeLater(() -> resolve(data));
        } else {
            resolve(data);
        }
    }

    private void resolve(String data) {
        if (logger != null) {
            logger.logResponse(data);
        }

        PsiFile source;
        if (project == null) {
            LispCoreProjectEnvironment projectEnvironment = new LispCoreProjectEnvironment();
            projectEnvironment.getEnvironment()
                    .registerParserDefinition(SltCommonLispLanguage.INSTANCE, new SltCommonLispParserDefinition());
            PsiFileFactory factory = PsiFileFactory.getInstance(projectEnvironment.getProject());
            source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        } else {
            PsiFileFactory factory = PsiFileFactory.getInstance(project);
            source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        }

        List<LispElement> elements = parse(data);
        if (elements.size() == 1) {
            LispElement element = elements.get(0);
            if (element instanceof LispList) {
                LispList reply = (LispList) element;
                if (isReturn(reply)) {
                    processReturn(reply);
                }
            }
        }
    }

    private List<LispElement> parse(String data) {
        PsiFile source;
        if (project == null) {
            LispCoreProjectEnvironment projectEnvironment = new LispCoreProjectEnvironment();
            projectEnvironment.getEnvironment()
                    .registerParserDefinition(SltCommonLispLanguage.INSTANCE, new SltCommonLispParserDefinition());
            PsiFileFactory factory = PsiFileFactory.getInstance(projectEnvironment.getProject());
            source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        } else {
            PsiFileFactory factory = PsiFileFactory.getInstance(project);
            source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        }

        return LispUtils.convertAst(source);
    }

    private boolean isReturn(LispList reply) {
        return reply.getItems().size() > 0 &&
                reply.getItems().get(0) instanceof LispSymbol &&
                ":return".equals(((LispSymbol) reply.getItems().get(0)).getValue());
    }

    private void processReturn(LispList reply) {
        LispInteger replyId = (LispInteger) reply.getItems().get(2);
        try {
            SlimeRequest request = requests.get(replyId.getValue());
            if (request instanceof SltEval) {
                SltEval eval = (SltEval) request;
                eval.processReply((LispList) reply.getItems().get(1));
            }

            if (request instanceof SwankEvalAndGrab) {
                SwankEvalAndGrab evalAndGrab = (SwankEvalAndGrab) request;
                evalAndGrab.processReply((LispList) reply.getItems().get(1), this::parse);
            }
        } finally {
            requests.remove(replyId.getValue());
        }
    }

    public interface RequestResponseLogger {

        void logRequest(String request);
        void logResponse(String response);

    }

}

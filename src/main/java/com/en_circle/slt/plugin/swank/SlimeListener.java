package com.en_circle.slt.plugin.swank;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltCommonLispParserDefinition;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.lisp.psi.LispCoreProjectEnvironment;
import com.en_circle.slt.plugin.swank.debug.SltDebugInfo;
import com.en_circle.slt.plugin.swank.requests.SltEval;
import com.en_circle.slt.plugin.swank.requests.SltFrameLocalsAndCatchTags;
import com.en_circle.slt.plugin.swank.requests.SltInvokeNthRestart;
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
    private final DebugInterface debugInterface;

    public SlimeListener(Project project, boolean fromUi, RequestResponseLogger logger, DebugInterface debugInterface) {
        this.project = project;
        this.fromUi = fromUi;
        this.logger = logger;
        this.debugInterface = debugInterface;
    }

    public void call(SlimeRequest request, SwankClient client) {
        BigInteger requestId = request.getRequestId() == null ? nextRpc() : request.getRequestId();
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
            if (element instanceof LispContainer) {
                LispContainer reply = (LispContainer) element;
                if (isReturn(reply)) {
                    processReturn(reply);
                } else if (isDebug(reply)) {
                    processDebug(reply);
                } else if (isDebugReturn(reply)) {
                    processDebugReturn(reply);
                } else if (isDebugActivate(reply)) {
                    processDebugActivate(reply);
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

    private boolean isReturn(LispContainer reply) {
        return reply.getItems().size() > 0 &&
                reply.getItems().get(0) instanceof LispSymbol &&
                ":return".equals(((LispSymbol) reply.getItems().get(0)).getValue());
    }

    private void processReturn(LispContainer reply) {
        LispInteger replyId = (LispInteger) reply.getItems().get(2);
        try {
            SlimeRequest request = requests.get(replyId.getValue());
            if (request instanceof SltEval) {
                SltEval eval = (SltEval) request;
                eval.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof SwankEvalAndGrab) {
                SwankEvalAndGrab evalAndGrab = (SwankEvalAndGrab) request;
                evalAndGrab.processReply((LispContainer) reply.getItems().get(1), this::parse);
            }

            if (request instanceof SltInvokeNthRestart) {
                SltInvokeNthRestart restart = (SltInvokeNthRestart) request;
                restart.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof SltFrameLocalsAndCatchTags) {
                SltFrameLocalsAndCatchTags frames = (SltFrameLocalsAndCatchTags) request;
                frames.processReply((LispContainer) reply.getItems().get(1));
            }
        } finally {
            requests.remove(replyId.getValue());
        }
    }

    private boolean isDebug(LispContainer reply) {
        return reply.getItems().size() > 0 &&
                reply.getItems().get(0) instanceof LispSymbol &&
                ":debug".equals(((LispSymbol) reply.getItems().get(0)).getValue());
    }

    private void processDebug(LispContainer reply) {
        SltDebugInfo debugInfo = new SltDebugInfo(reply);
        if (debugInterface != null) {
            debugInterface.onDebugCreate(debugInfo);
        }
    }

    private void processDebugReturn(LispContainer reply) {
        if (debugInterface != null) {
            BigInteger threadId = ((LispInteger) reply.getItems().get(1)).getValue();
            BigInteger level = ((LispInteger) reply.getItems().get(1)).getValue();
            debugInterface.onDebugReturn(threadId, level);
        }
    }

    private void processDebugActivate(LispContainer reply) {
        if (debugInterface != null) {
            BigInteger threadId = ((LispInteger) reply.getItems().get(1)).getValue();
            BigInteger level = ((LispInteger) reply.getItems().get(1)).getValue();
            debugInterface.onDebugActivate(threadId, level);
        }
    }

    private boolean isDebugReturn(LispContainer reply) {
        return reply.getItems().size() > 0 &&
                reply.getItems().get(0) instanceof LispSymbol &&
                ":debug-return".equals(((LispSymbol) reply.getItems().get(0)).getValue());
    }

    private boolean isDebugActivate(LispContainer reply) {
        return reply.getItems().size() > 0 &&
                reply.getItems().get(0) instanceof LispSymbol &&
                ":debug-activate".equals(((LispSymbol) reply.getItems().get(0)).getValue());
    }

    public interface RequestResponseLogger {

        void logRequest(String request);
        void logResponse(String response);

    }

    public interface DebugInterface {

        void onDebugCreate(SltDebugInfo info);

        void onDebugActivate(BigInteger debugId, BigInteger level);

        void onDebugReturn(BigInteger debugId, BigInteger level);

    }

}

package com.en_circle.slt.plugin.swank;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltCommonLispParserDefinition;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.lisp.psi.LispCoreProjectEnvironment;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.debug.SltDebugInfo;
import com.en_circle.slt.plugin.swank.requests.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;

import java.math.BigInteger;
import java.util.*;
import java.util.function.Consumer;

public class SlimeListener implements SwankClient.SwankReply {

    private static BigInteger rpcIdentifier = BigInteger.ZERO;

    public static synchronized BigInteger nextRpc() {
        BigInteger next = rpcIdentifier.add(BigInteger.ONE);
        rpcIdentifier = next;
        return next;
    }

    private final Project project;
    private final boolean fromUi;
    private final Map<BigInteger, SlimeRequest> requests = Collections.synchronizedMap(new HashMap<>());
    private final Consumer<Exception> onReadFailure;
    private final RequestResponseLogger logger;
    private final DebugInterface debugInterface;

    public SlimeListener(Project project, boolean fromUi, Consumer<Exception> onReadFailure, RequestResponseLogger logger, DebugInterface debugInterface) {
        this.project = project;
        this.fromUi = fromUi;
        this.onReadFailure = onReadFailure;
        this.logger = logger;
        this.debugInterface = debugInterface;
    }

    public void call(SlimeRequest request, SwankClient client) {
        BigInteger requestId = request.getRequestId() == null ? nextRpc() : request.getRequestId();
        requests.put(requestId, request);
        SwankPacket packet = request.createPacket(requestId, project);
        if (logger != null) {
            logger.logRequest(packet.getSentData());
        }
        client.swankSend(packet);
    }

    @Override
    public void onSwankMessage(SwankPacket packet) {
        String data = packet.getSentData();

        if (fromUi) {
            ApplicationManager.getApplication().executeOnPooledThread(() ->
                    ApplicationManager.getApplication().runReadAction(() -> resolve(data)));
        } else {
            resolve(data);
        }
    }

    @Override
    public void onReadError(Exception e) {

    }

    private void resolve(String data) {
        if (logger != null) {
            logger.logResponse(data);
        }

        List<LispElement> elements = parse(data);
        if (elements.size() == 1) {
            LispElement element = elements.get(0);
            if (element instanceof LispContainer reply) {
                if (isReturn(reply)) {
                    processReturn(reply);
                } else if (isDebug(reply)) {
                    processDebug(reply);
                } else if (isDebugReturn(reply)) {
                    processDebugReturn(reply);
                } else if (isDebugActivate(reply)) {
                    processDebugActivate(reply);
                } else if (isIndentation(reply)) {
                    LispEnvironmentService.getInstance(project).updateIndentation(reply.getItems().get(1));
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
            source = factory.createFileFromText(UUID.randomUUID() + "swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        } else {
            PsiFileFactory factory = PsiFileFactory.getInstance(project);
            source = factory.createFileFromText(UUID.randomUUID() + "swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
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
            if (request instanceof Eval eval) {
                eval.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof EvalAndGrab evalAndGrab) {
                evalAndGrab.processReply((LispContainer) reply.getItems().get(1), this::parse);
            }

            if (request instanceof InvokeNthRestart restart) {
                restart.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof FrameLocalsAndCatchTags frames) {
                frames.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof InspectFrameVar frameVar) {
                frameVar.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof InspectNth inspectNth) {
                inspectNth.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof InspectorAction action) {
                action.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof StepperAction action) {
                action.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof MacroexpandAll macroexpandAll) {
                macroexpandAll.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof Macroexpand macroexpand) {
                macroexpand.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof Macroexpand1 macroexpand1) {
                macroexpand1.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof SimpleCompletion simpleCompletion) {
                simpleCompletion.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof Xrefs xrefs) {
                xrefs.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof CompleteSearch completeSearch) {
                completeSearch.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof ListThreads listThreads) {
                listThreads.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof KillThread killThread) {
                killThread.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof SuspendThread suspendThread) {
                suspendThread.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof Argslist argslist) {
                argslist.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof InspectSymbol action) {
                action.processReply((LispContainer) reply.getItems().get(1));
            }

            if (request instanceof Disassemble disassemble) {
                disassemble.processReply((LispContainer) reply.getItems().get(1));
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

    private boolean isIndentation(LispContainer reply) {
        return reply.getItems().size() > 0 &&
                reply.getItems().get(0) instanceof LispSymbol &&
                ":indentation-update".equals(((LispSymbol) reply.getItems().get(0)).getValue());
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

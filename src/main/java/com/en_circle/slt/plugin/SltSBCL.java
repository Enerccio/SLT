package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.SymbolState.SymbolBinding;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElementType;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.*;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerListener;
import com.en_circle.slt.plugin.swank.requests.SwankEvalAndGrab;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.FileContentUtilCore;

import java.io.IOException;
import java.util.*;

public class SltSBCL {

    private static final SltSBCL currentState = new SltSBCL();

    public static SltSBCL getInstance() {
        return currentState;
    }

    private SwankClient client;
    private SlimeListener slimeListener;
    private Project project;
    private RequestResponseLogger logger;
    private DebugInterface debugInterface;
    private final List<SBCLServerListener> serverListeners = Collections.synchronizedList(new ArrayList<>());
    private final Map<String, SymbolState> symbolInformation = Collections.synchronizedMap(new HashMap<>());

    public void start() throws Exception {
        for (SBCLServerListener listener : serverListeners) {
            listener.onPreStart();
        }

        SwankServerConfiguration c = new SwankServerConfiguration.Builder()
                .setExecutable(SltState.getInstance().sbclExecutable)
                .setPort(SltState.getInstance().port)
                .setQuicklispStartScriptPath(SltState.getInstance().quicklispStartScript)
                .setProjectDirectory(project.getBasePath())
                .setListener((output, newData) -> {
                    for (SBCLServerListener listener : serverListeners) {
                        listener.onOutputChanged(output, newData);
                    }
                })
                .build();
        SwankServer.startSbcl(c);

        slimeListener = new SlimeListener(project, true, logger, debugInterface);
        client = new SwankClient("127.0.0.1", SltState.getInstance().port, slimeListener);

        for (SBCLServerListener listener : serverListeners) {
            listener.onPostStart();
        }
    }

    public void setRequestResponseLogger(RequestResponseLogger logger) {
        this.logger = logger;
    }

    public void setDebugInterface(DebugInterface debugInterface) {
        this.debugInterface = debugInterface;
    }

    public void sendToSbcl(SlimeRequest request) throws Exception {
        sendToSbcl(request, true);
    }

    public void sendToSbcl(SlimeRequest request, boolean startServer) throws Exception {
        if (startServer && !SwankServer.INSTANCE.isActive()) {
            start();
        }
        if (!SwankServer.INSTANCE.isActive()) {
            if (!startServer)
                return; // ignored
            throw new IOException("server offline");
        }

        if (slimeListener != null) {
            slimeListener.call(request, client);
        }
    }

    public void stop() throws Exception {
        for (SBCLServerListener listener : serverListeners) {
            listener.onPreStop();
        }
        try {
            client.close();
        } finally {
            SwankServer.stop();
            symbolInformation.clear();
        }

        for (SBCLServerListener listener : serverListeners) {
            listener.onPostStop();
        }
    }

    public void addServerListener(SBCLServerListener listener) {
        serverListeners.add(listener);
    }

    public Project getProject() {
        return project;
    }

    public void setProject(Project project) {
        this.project = project;
    }

    public String getGlobalPackage() {
        return "cl-user";
    }

    public SymbolState getOrCreateBinding(String packageName, String symbolName) {
        return symbolInformation.computeIfAbsent(packageName + ":" + symbolName, SymbolState::new);
    }

    public SymbolState refreshSymbolFromServer(String packageName, String symbolName, PsiElement element) {
        SymbolState state = getOrCreateBinding(packageName, symbolName);
        SymbolBinding currentBinding = state.binding;
        try {
            if (currentBinding == SymbolBinding.NONE || cacheInvalid(state)) {
                sendToSbcl(SwankEvalAndGrab.eval(
                        String.format(
                                "(slt-core:analyze-symbol '%s)",
                                symbolName),
                        SltSBCL.getInstance().getGlobalPackage(), true, (result, stdout, parsed) -> {
                            if (parsed.size() == 1 && parsed.get(0).getType() == LispElementType.CONTAINER) {
                                LispContainer list = (LispContainer) parsed.get(0);
                                String symValue = ((LispSymbol) list.getItems().get(0)).getValue().toUpperCase();
                                boolean changed = false;
                                state.timestamp = System.currentTimeMillis();
                                switch (symValue) {
                                    case ":SPECIAL-FORM":
                                        changed |= state.binding != SymbolBinding.SPECIAL_FORM;
                                        state.binding = SymbolBinding.SPECIAL_FORM;
                                        break;
                                    case ":MACRO":
                                        changed |= state.binding != SymbolBinding.MACRO;
                                        state.binding = SymbolBinding.MACRO;
                                        break;
                                    case ":FUNCTION":
                                        changed |= state.binding != SymbolBinding.FUNCTION;
                                        state.binding = SymbolBinding.FUNCTION;
                                        break;
                                    case ":CONSTANT":
                                        changed |= state.binding != SymbolBinding.CONSTANT;
                                        state.binding = SymbolBinding.CONSTANT;
                                        break;
                                    case ":KEYWORD":
                                        changed |= state.binding != SymbolBinding.KEYWORD;
                                        state.binding = SymbolBinding.KEYWORD;
                                        break;
                                    case ":SPECIAL":
                                        changed |= state.binding != SymbolBinding.SPECIAL_VARIABLE;
                                        state.binding = SymbolBinding.SPECIAL_VARIABLE;
                                        break;
                                    default:
                                        changed |= state.binding != SymbolBinding.NONE;
                                        state.binding = SymbolBinding.NONE;
                                        break;
                                }
                                state.symbol = packageName + ":" + symbolName;
                                boolean hasDoc = state.documentation != null;
                                state.documentation = null;
                                if (list.getItems().get(1) instanceof LispString) {
                                    state.documentation = ((LispString) list.getItems().get(1)).getValue();
                                    if (!hasDoc) {
                                        changed = true;
                                    }
                                }

                                if (changed) {
                                    PsiFile file = element.getContainingFile();
                                    FileContentUtilCore.reparseFiles(file.getVirtualFile());
                                }
                            }
                        }), false);
            }
        } catch (Exception e) {
            // TODO
        }
        return state;
    }

    private boolean cacheInvalid(SymbolState state) {
        if (state.timestamp == null)
            return false;
        if (System.currentTimeMillis() - state.timestamp > 10000)
            return false;

        return true;
    }

    public interface SBCLServerListener extends SwankServerListener {

        void onPreStart();
        void onPostStart();
        void onPreStop();
        void onPostStop();

    }

}

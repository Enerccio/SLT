package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.SymbolState.SymbolBinding;
import com.en_circle.slt.plugin.lisp.lisp.LispElementType;
import com.en_circle.slt.plugin.lisp.lisp.LispList;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeListener;
import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankClient;
import com.en_circle.slt.plugin.swank.SwankServer;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerListener;
import com.en_circle.slt.plugin.swank.requests.SwankEvalAndGrab;
import com.intellij.openapi.project.Project;

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
    private final List<SBCLServerListener> serverListeners = Collections.synchronizedList(new ArrayList<>());
    private final Map<String, SymbolState> symbolInformation = Collections.synchronizedMap(new HashMap<>());

    public void start() throws Exception {
        for (SBCLServerListener listener : serverListeners) {
            listener.onPreStart();
        }

        SwankServer.startSbcl(SltState.getInstance().sbclExecutable, SltState.getInstance().port,
                (output, newData) -> {
                    for (SBCLServerListener listener : serverListeners) {
                        listener.onOutputChanged(output, newData);
                    }
                });

        slimeListener = new SlimeListener(project, true, logger);
        client = new SwankClient("127.0.0.1", SltState.getInstance().port, slimeListener);

        for (SBCLServerListener listener : serverListeners) {
            listener.onPostStart();
        }
    }

    public void setRequestResponseLogger(RequestResponseLogger logger) {
        this.logger = logger;
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
        return symbolInformation.computeIfAbsent(packageName + ":" + symbolName, x -> new SymbolState(x));
    }

    public SymbolState refreshSymbolFromServer(String packageName, String symbolName) {
        SymbolState state = getOrCreateBinding(packageName, symbolName);
        SymbolBinding currentBinding = state.binding;
        try {
            if (currentBinding == SymbolBinding.NONE || checkCache(state)) {
                sendToSbcl(SwankEvalAndGrab.eval(
                        String.format(
                                "(let ((test-sym '%s) (*standard-output* (make-string-output-stream))) \n" +
                                        "   (cond \n" +
                                        "       ((special-operator-p test-sym) (list :special-form NIL))\n" +
                                        "       ((macro-function test-sym) (progn \n" +
                                        "                                       (describe test-sym) \n" +
                                        "                                       (list :macro (get-output-stream-string *standard-output*))))\n" +
                                        "       ((fboundp test-sym) (progn \n " +
                                        "                               (describe test-sym) \n" +
                                        "                               (list :function (get-output-stream-string *standard-output*))))\n" +
                                        "       (T (list NIL NIL))))",
                                symbolName),
                        SltSBCL.getInstance().getGlobalPackage(), true, (result, stdout, parsed) -> {
                            if (parsed.size() == 1 && parsed.get(0).getType() == LispElementType.LIST) {
                                LispList list = (LispList) parsed.get(0);
                                String symValue = ((LispSymbol) list.getItems().get(0)).getValue().toUpperCase();
                                switch (symValue) {
                                    case ":SPECIAL-FORM":
                                        state.binding = SymbolBinding.SPECIAL_FORM;
                                        break;
                                    case ":MACRO":
                                        state.binding = SymbolBinding.MACRO;
                                        break;
                                    case ":FUNCTION":
                                        state.binding = SymbolBinding.FUNCTION;
                                        break;
                                    default:
                                        state.binding = SymbolBinding.NONE;
                                        break;
                                }
                                state.documentation = null;
                                if (list.getItems().get(1) instanceof LispString) {
                                    state.documentation = ((LispString) list.getItems().get(1)).getValue();
                                }
                            }
                        }), false);
            }
        } catch (Exception e) {
            // TODO
        }
        return state;
    }

    private boolean checkCache(SymbolState state) {
        // TODO?
        return false;
    }

    public interface SBCLServerListener extends SwankServerListener {

        void onPreStart();
        void onPostStart();
        void onPreStop();
        void onPostStop();

    }

}

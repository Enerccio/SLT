package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.swank.*;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerListener;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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
            SltSBCLSymbolCache.INSTANCE.clear();
            SwankServer.stop();
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

    public SymbolState refreshSymbolFromServer(String packageName, String symbolName, PsiElement element) {
        return SltSBCLSymbolCache.INSTANCE.refreshSymbolFromServer(packageName, symbolName, element);
    }

    public boolean hasEventsSet() {
        return logger != null;
    }

    public interface SBCLServerListener extends SwankServerListener {

        void onPreStart();
        void onPostStart();
        void onPreStop();
        void onPostStop();

    }

}

package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.swank.SlimeListener;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankClient;
import com.en_circle.slt.plugin.swank.SwankServer;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerListener;
import com.intellij.openapi.project.Project;

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
    private final List<SBCLServerListener> serverListeners = Collections.synchronizedList(new ArrayList<>());

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

        slimeListener = new SlimeListener(project, true);
        client = new SwankClient("127.0.0.1", SltState.getInstance().port, slimeListener);

        for (SBCLServerListener listener : serverListeners) {
            listener.onPostStart();
        }
    }

    public void sendToSbcl(SlimeRequest request) throws Exception {
        sendToSbcl(request, true);
    }

    public void sendToSbcl(SlimeRequest request, boolean startServer) throws Exception {
        if (startServer && !SwankServer.INSTANCE.isActive()) {
            start();
        }
        if (!SwankServer.INSTANCE.isActive())
            throw new IOException("server offline");

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

    public interface SBCLServerListener extends SwankServerListener {
        void onPreStart();
        void onPostStart();
        void onPreStop();
        void onPostStop();

    }

}

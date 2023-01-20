package com.en_circle.slt.plugin.services.lisp;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltState;
import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.environment.*;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.services.lisp.components.SltIndentationContainer;
import com.en_circle.slt.plugin.services.lisp.components.SltLispEnvironmentMacroExpandCache;
import com.en_circle.slt.plugin.services.lisp.components.SltLispEnvironmentSymbolCache;
import com.en_circle.slt.plugin.swank.SlimeListener;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankClient;
import com.en_circle.slt.tools.ProjectUtils;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.psi.PsiElement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Supplier;

public class LispEnvironmentServiceImpl implements LispEnvironmentService {
    private static final Logger log = LoggerFactory.getLogger(LispEnvironmentServiceImpl.class);

    private Supplier<SltLispEnvironment> environmentProvider;
    private SltLispEnvironment environment;
    private SltLispEnvironmentConfiguration.Builder<?, ?> configurationBuilder;
    private SltLispEnvironmentConfiguration configuration;
    private SwankClient client;
    private SlimeListener slimeListener;
    private RequestResponseLogger logger;
    private DebugInterface debugInterface;
    private final List<LispEnvironmentListener> serverListeners = Collections.synchronizedList(new ArrayList<>());
    private volatile boolean starting = false;

    private Project project;
    private SltIndentationContainer indentationContainer;
    private SltLispEnvironmentSymbolCache symbolCache;
    private SltLispEnvironmentMacroExpandCache macroExpandCache;

    public synchronized boolean initProject(Project project) {
        if (this.project == null) {
            indentationContainer = new SltIndentationContainer();
            symbolCache = new SltLispEnvironmentSymbolCache(project);
            macroExpandCache = new SltLispEnvironmentMacroExpandCache();
            this.project = project;
            return true;
        }
        return false;
    }

    public void postInit() {
        symbolCache.start();
    }

    @Override
    public void resetConfiguration() {
        this.configurationBuilder = null;
    }

    public boolean configured() {
        environmentProvider = SltSBCLEnvironment::new;
        configurationBuilder = new SltSBCLEnvironmentConfiguration.Builder()
                .setExecutable(SltState.getInstance().sbclExecutable)
                .setPort(SltState.getInstance().port)
                .setQuicklispStartScriptPath(SltState.getInstance().quicklispStartScript)
                .setProjectDirectory(ProjectUtils.getCurrentProject().getBasePath());

        // add validation checks and custom "SDK"

        return true;
    }

    @Override
    public void addServerListener(LispEnvironmentListener listener) {
        serverListeners.add(listener);
    }

    @Override
    public void setRequestResponseLogger(RequestResponseLogger logger) {
        this.logger = logger;
    }

    @Override
    public void setDebugInterface(DebugInterface debugInterface) {
        this.debugInterface = debugInterface;
    }

    @Override
    public void start() {
        starting = true;
        ApplicationManager.getApplication().invokeLater(() -> {
            try {
                ensureToolWindowIsOpen();
                doStart();
            } catch (Exception e) {
                log.warn(SltBundle.message("slt.error.sbclstart"), e);
                Messages.showErrorDialog(ProjectUtils.getCurrentProject(), e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
            }
        });
    }

    private void ensureToolWindowIsOpen() {
        ToolWindow toolWindow = ToolWindowManager.getInstance(ProjectUtils.getCurrentProject())
                .getToolWindow("Common Lisp");
        assert toolWindow != null;
        toolWindow.show();
    }

    @Override
    public void stop() {
        ApplicationManager.getApplication().invokeLater(() -> {
            try {
                doStop();
            } catch (Exception e) {
                log.warn(SltBundle.message("slt.error.sbclstop"), e);
                Messages.showErrorDialog(ProjectUtils.getCurrentProject(), e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.stop"));
            }
        });
    }

    private void doStart() throws Exception {
        try {
            if (configurationBuilder == null) {
                if (!configured()) {
                    log.warn(SltBundle.message("slt.error.sbclstart"));
                    Messages.showErrorDialog(ProjectUtils.getCurrentProject(),
                            SltBundle.message("slt.ui.errors.sbcl.start.noconf"),
                            SltBundle.message("slt.ui.errors.sbcl.start"));
                    return;
                }
            }

            for (LispEnvironmentListener listener : serverListeners) {
                listener.onPreStart();
            }

            if (configuration == null) {
                configuration = configurationBuilder
                        .setListener(this::onServerOutput)
                        .build();
            }
            environment = environmentProvider.get();
            environment.start(configuration);

            slimeListener = new SlimeListener(ProjectUtils.getCurrentProject(), true, logger, debugInterface);
            client = new SwankClient("127.0.0.1", SltState.getInstance().port, slimeListener);

            for (LispEnvironmentListener listener : serverListeners) {
                listener.onPostStart();
            }
        } finally {
            starting = false;
        }
    }

    private void onServerOutput(SltOutput output, String newData) {
        for (LispEnvironmentListener listener : serverListeners) {
            listener.onOutputChanged(output, newData);
        }
    }

    private void doStop() throws Exception {
        for (LispEnvironmentListener listener : serverListeners) {
            listener.onPreStop();
        }
        try {
            client.close();
        } finally {
            indentationContainer.clear();
            indentationContainer.clear();
            symbolCache.clear();
            if (environment != null) {
                environment.stop();
                environment = null;
            }
        }

        for (LispEnvironmentListener listener : serverListeners) {
            listener.onPostStop();
        }
    }

    @Override
    public void sendToLisp(SlimeRequest request) throws Exception {
        sendToLisp(request, true);
    }

    @Override
    public void sendToLisp(SlimeRequest request, boolean startServer) throws Exception {
        if (startServer && environment == null || !environment.isActive()) {
            ApplicationManager.getApplication().invokeLater(() -> {
                starting = true;
                ApplicationManager.getApplication().invokeLater(() -> {
                    try {
                        ensureToolWindowIsOpen();
                        doStart();
                        doSend(request);
                    } catch (Exception e) {
                        log.warn(SltBundle.message("slt.error.sbclstart"), e);
                        Messages.showErrorDialog(ProjectUtils.getCurrentProject(), e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
                    }
                });
            });
            return;
        }

        if (environment == null || !environment.isActive()) {
            if (!startServer)
                return; // ignored
            throw new SltProcessException("server offline");
        }

        doSend(request);
    }

    private void doSend(SlimeRequest request) {
        if (slimeListener != null) {
            slimeListener.call(request, client);
        }
    }

    @Override
    public String getGlobalPackage() {
        return "CL-USER";
    }

    @Override
    public SymbolState refreshSymbolFromServer(String packageName, String symbolName, PsiElement element) {
        return symbolCache.refreshSymbolFromServer(packageName, symbolName, element);
    }

    @Override
    public LispEnvironmentState getState() {
        if (starting) {
            return LispEnvironmentState.INITIALIZING;
        }
        if (environment != null && environment.isActive()) {
            return LispEnvironmentState.READY;
        }
        return LispEnvironmentState.STOPPED;
    }

    @Override
    public SltLispEnvironment getEnvironment() {
        return environment;
    }

    @Override
    public String macroexpand(LispList form, String packageName) {
        try {
            return macroExpandCache.macroexpand(form, packageName);
        } catch (Exception e) {
            log.error(e.getMessage());
            log.debug(e.getMessage(), e);
        }
        return null;
    }

    @Override
    public void updateIndentation(LispElement element) {
        indentationContainer.update((LispContainer) element);
    }

    @Override
    public void dispose() {
        try {
            doStop();
        } catch (Exception ignored) {

        }
        if (symbolCache != null)
            symbolCache.terminate();
    }

}

package com.en_circle.slt.plugin.services.lisp;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.environment.LispFeatures;
import com.en_circle.slt.plugin.environment.SltLispEnvironment;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.environment.SltLispEnvironmentConfiguration;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.sdk.LispProjectSdk;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.sdk.SdkList;
import com.en_circle.slt.plugin.services.lisp.components.*;
import com.en_circle.slt.plugin.services.lisp.components.SltLispEnvironmentSymbolCache.BatchedSymbolRefreshAction;
import com.en_circle.slt.plugin.swank.SlimeListener;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankClient;
import com.en_circle.slt.plugin.ui.debug.SltBreakpointProperties;
import com.en_circle.slt.plugin.ui.debug.SltSymbolBreakpointType;
import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.hints.ParameterHintsPassFactory;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.ExceptionUtil;
import com.intellij.xdebugger.XDebuggerManager;
import com.intellij.xdebugger.breakpoints.XBreakpoint;
import com.intellij.xdebugger.breakpoints.XBreakpointManager;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
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
    private LispSltOverrides overrides;
    private volatile boolean starting = false;

    private final Project project;
    private final SltIndentationContainer indentationContainer;
    private final SltLispEnvironmentSymbolCache symbolCache;
    private final SltLispEnvironmentMacroExpandCache macroExpandCache;
    private final SltBreakpointContainer breakpointContainer;

    public LispEnvironmentServiceImpl(Project project) {
        this.project = project;
        indentationContainer = new SltIndentationContainer();
        indentationContainer.init(project);
        symbolCache = new SltLispEnvironmentSymbolCache(project);
        macroExpandCache = new SltLispEnvironmentMacroExpandCache();
        breakpointContainer = new SltBreakpointContainer(project);
        symbolCache.start();

        addServerListener(breakpointContainer);
    }

    @Override
    public void resetConfiguration() {
        this.configurationBuilder = null;
        this.environmentProvider = null;
        this.configuration = null;
    }

    public boolean configured() {
        LispProjectSdk projectSdk = LispProjectSdk.getInstance(project);
        if (projectSdk.currentSDK == null) {
            return false;
        }
        SdkList list = SdkList.getInstance();
        LispSdk sdk = list.getSdkByUuid(projectSdk.currentSDK);
        if (sdk == null) {
            return false;
        }

        environmentProvider = sdk.getEnvironment().getDefinition().getEnvironmentCreator();
        configurationBuilder = sdk.getEnvironment().getDefinition().buildConfiguration(sdk, project);

        // TODO: add UI resolve actions here

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
        ApplicationManager.getApplication().executeOnPooledThread(() -> {
            starting = true;
            ApplicationManager.getApplication().invokeAndWait(this::ensureToolWindowIsOpen);
            ApplicationManager.getApplication().executeOnPooledThread(() -> {
                try {
                    doStart();
                } catch (Exception e) {
                    log.warn(SltBundle.message("slt.error.start"), e);
                    ApplicationManager.getApplication().invokeLater(() ->
                            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start")));
                }
            });
        });
    }

    private void ensureToolWindowIsOpen() {
        ToolWindow toolWindow = ToolWindowManager.getInstance(project)
                .getToolWindow("Common Lisp");
        assert toolWindow != null;
        toolWindow.show();
    }

    @Override
    public void stop() {
        try {
            doStop();
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.stop"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.lisp.stop"));
        }
    }

    private boolean doStart() throws Exception {
        try {
            if (configurationBuilder == null) {
                AtomicBoolean state = new AtomicBoolean(false);
                ApplicationManager.getApplication().invokeAndWait(() -> {
                    if (!configured()) {
                        log.warn(SltBundle.message("slt.error.start"));
                        Messages.showErrorDialog(project,
                                SltBundle.message("slt.ui.errors.lisp.start.noconf"),
                                SltBundle.message("slt.ui.errors.lisp.start"));
                        return;
                    }
                    state.set(true);
                });
                if (!state.get()) {
                    return state.get();
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
            if (environment != null) {
                overrides = environment.getType().getDefinition().getOverrides();

                slimeListener = new SlimeListener(project, true, e -> {
                    for (LispEnvironmentListener listener : serverListeners) {
                        String text = ExceptionUtil.getThrowableText(e);
                        listener.onOutputChanged(SltOutput.STDERR, text);
                    }
                }, logger, debugInterface);
                client = new SwankClient("127.0.0.1", environment.getSwankPort(), slimeListener);

                for (LispEnvironmentListener listener : serverListeners) {
                    listener.onPostStart();
                }

                ApplicationManager.getApplication().invokeLaterOnWriteThread(() -> {
                    ParameterHintsPassFactory.forceHintsUpdateOnNextPass();
                    DaemonCodeAnalyzer.getInstance(project).restart();

                    XBreakpointManager breakpointManager = XDebuggerManager.getInstance(project).getBreakpointManager();
                    for (XLineBreakpoint<SltBreakpointProperties> breakpoint :
                            breakpointManager.getBreakpoints(SltSymbolBreakpointType.class)) {
                        addBreakpoint(breakpoint);
                    }
                });
            }
        } finally {
            starting = false;
        }
        return true;
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
            if (client != null)
                client.close();
        } finally {
            overrides = null;
            indentationContainer.clear();
            indentationContainer.clear();
            symbolCache.clear();
            if (environment != null) {
                environment.stop();
                environment = null;
            }
        }

        ApplicationManager.getApplication().invokeLaterOnWriteThread(() -> {
            ParameterHintsPassFactory.forceHintsUpdateOnNextPass();
            DaemonCodeAnalyzer.getInstance(project).restart();
        });

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
        sendToLisp(request, startServer, null);
    }

    @Override
    public void sendToLisp(SlimeRequest request, boolean startServer, Runnable onFailureServerState) throws Exception {
        ApplicationManager.getApplication().executeOnPooledThread(() -> {
            if (environment == null || !environment.isActive()) {
                if (startServer) {
                    starting = true;
                    ApplicationManager.getApplication().invokeAndWait(this::ensureToolWindowIsOpen);
                    ApplicationManager.getApplication().executeOnPooledThread(() -> {
                        try {
                            if (!doStart()) {
                                if (onFailureServerState != null)
                                    onFailureServerState.run();
                                return;
                            }

                            doSend(request);
                        } catch (Exception e) {
                            log.warn(SltBundle.message("slt.error.start"), e);
                            ApplicationManager.getApplication().invokeLater(() ->
                                    Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start")));
                        }
                    });
                } else {
                    if (onFailureServerState != null)
                        onFailureServerState.run();
                }
            } else if (!starting) {
                doSend(request);
            }
        });
    }

    private void doSend(SlimeRequest request) {
        if (slimeListener != null) {
            slimeListener.call(request, client);
        }
    }

    @Override
    public String getGlobalPackage() {
        return "COMMON-LISP-USER";
    }

    @Override
    public SymbolState refreshSymbolFromServer(String packageName, String symbolName) {
        return symbolCache.refreshSymbolFromServer(packageName, symbolName);
    }

    @Override
    public BatchedSymbolRefreshAction refreshSymbolsFromServer() {
        return symbolCache.createNewBatch();
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

        // also clear macro cache since we get this hit on macro update
        macroExpandCache.clear();
    }

    @Override
    public Integer calculateOffset(PsiElement element, PsiFile file, boolean wasAfter, String text, int offset, String packageOverride) {
        return indentationContainer.calculateIndent(element, file, wasAfter, text, offset, packageOverride);
    }

    @Override
    public void addBreakpoint(XBreakpoint<SltBreakpointProperties> nativeBreakpoint) {
        breakpointContainer.addBreakpoint(nativeBreakpoint);
    }

    @Override
    public void removeBreakpoint(XBreakpoint<SltBreakpointProperties> nativeBreakpoint) {
        breakpointContainer.removeBreakpoint(nativeBreakpoint);
    }

    @Override
    public void nativeBreakpointUpdated(XBreakpoint<SltBreakpointProperties> nativeBreakpoint) {
        breakpointContainer.onUpdate(nativeBreakpoint);
    }

    @Override
    public Collection<SltBreakpoint> getAllBreakpoints() {
        return breakpointContainer.getAllBreakpoints();
    }

    @Override
    public LispSltOverrides getOverrides() {
        return overrides;
    }

    @Override
    public boolean hasFeature(LispFeatures feature) {
        if (environment != null) {
            return environment.getType().getDefinition().hasFeature(feature);
        }
        return false;
    }

    @Override
    public String getBreakpointsForInstall() {
        if (hasFeature(LispFeatures.BREAKPOINTS))
            return breakpointContainer.getInstallBreakpoints();
        return null;
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

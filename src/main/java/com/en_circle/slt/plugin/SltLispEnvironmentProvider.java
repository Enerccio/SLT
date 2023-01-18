package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.environment.SltLispEnvironment;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltLispOutputChangedListener;
import com.en_circle.slt.plugin.environment.SltLispEnvironmentConfiguration;
import com.en_circle.slt.plugin.environment.SltProcessException;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.swank.SlimeListener;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankClient;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiElement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Supplier;

public class SltLispEnvironmentProvider implements Disposable {
    private static final Logger log = LoggerFactory.getLogger(SltLispEnvironmentProvider.class);

    private static final SltLispEnvironmentProvider INSTANCE = new SltLispEnvironmentProvider();

    public static SltLispEnvironmentProvider getInstance() {
        return INSTANCE;
    }

    private Supplier<SltLispEnvironment> environmentProvider;
    private SltLispEnvironment environment;
    private SltLispEnvironmentConfiguration.Builder<?, ?> configurationBuilder;
    private SltLispEnvironmentConfiguration configuration;

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

        if (configuration == null) {
            configuration = configurationBuilder
                    .setListener((output, newData) -> {
                        for (SBCLServerListener listener : serverListeners) {
                            listener.onOutputChanged(output, newData);
                        }
                    })
                    .build();
        }
        environment = environmentProvider.get();
        environment.start(configuration);

        slimeListener = new SlimeListener(project, true, logger, debugInterface);
        client = new SwankClient("127.0.0.1", SltState.getInstance().port, slimeListener);

        for (SBCLServerListener listener : serverListeners) {
            listener.onPostStart();
        }
    }

    public void setEnvironmentProvider(Supplier<SltLispEnvironment> environmentProvider) {
        this.environmentProvider = environmentProvider;
    }

    public void setConfigurationBuilder(SltLispEnvironmentConfiguration.Builder<?, ?> configurationBuilder) {
        this.configurationBuilder = configurationBuilder;
        this.configuration = null;
    }

    public void setRequestResponseLogger(RequestResponseLogger logger) {
        this.logger = logger;
    }

    public void setDebugInterface(DebugInterface debugInterface) {
        this.debugInterface = debugInterface;
    }

    public void sendToLisp(SlimeRequest request) throws Exception {
        sendToLisp(request, true);
    }

    public void sendToLisp(SlimeRequest request, boolean startServer) throws Exception {
        if (startServer && environment == null || !environment.isActive()) {
            start();
        }
        if (environment == null || !environment.isActive()) {
            if (!startServer)
                return; // ignored
            throw new SltProcessException("server offline");
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
            SltIndentationContainer.INSTANCE.clear();
            SltLispEnvironmentMacroExpandCache.INSTANCE.clear();
            SltLispEnvironmentSymbolCache.INSTANCE.clear();
            if (environment != null) {
                environment.stop();
                environment = null;
            }
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
        Disposer.register(project, this);
        this.project = project;
    }

    public String getGlobalPackage() {
        return "CL-USER";
    }

    public SymbolState refreshSymbolFromServer(String packageName, String symbolName, PsiElement element) {
        return SltLispEnvironmentSymbolCache.INSTANCE.refreshSymbolFromServer(packageName, symbolName, element);
    }

    public boolean hasEventsSet() {
        return logger != null;
    }

    public boolean isLispEnvironmentActive() {
        return environment != null && environment.isActive();
    }

    public SltLispEnvironment getEnvironment() {
        return environment;
    }

    @Override
    public void dispose() {
        try {
            stop();
        } catch (Exception e) {
            log.error(e.getMessage());
            log.debug(e.getMessage(), e);
        }
    }

    public String macroexpand(LispList form, String packageName) {
        try {
            return SltLispEnvironmentMacroExpandCache.INSTANCE.macroexpand(form, packageName);
        } catch (Exception e) {
            log.error(e.getMessage());
            log.debug(e.getMessage(), e);
        }
        return null;
    }

    public void updateIndentation(LispElement element) {
        SltIndentationContainer.INSTANCE.update((LispContainer) element);
    }

    public interface SBCLServerListener extends SltLispOutputChangedListener {

        void onPreStart();
        void onPostStart();
        void onPreStop();
        void onPostStop();

    }

}

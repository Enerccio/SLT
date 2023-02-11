package com.en_circle.slt.plugin.services.lisp;

import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.environment.LispFeatures;
import com.en_circle.slt.plugin.environment.SltLispEnvironment;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltLispOutputChangedListener;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.services.lisp.components.SltBreakpoint;
import com.en_circle.slt.plugin.services.lisp.components.SltLispEnvironmentSymbolCache.BatchedSymbolRefreshAction;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.ui.debug.SltBreakpointProperties;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.xdebugger.breakpoints.XBreakpoint;

import java.util.Collection;


public interface LispEnvironmentService extends Disposable {

    static LispEnvironmentService getInstance(Project project) {
        return project.getService(LispEnvironmentService.class);
    }

    void resetConfiguration();

    void addServerListener(LispEnvironmentListener listener);

    void setRequestResponseLogger(RequestResponseLogger logger);

    void setDebugInterface(DebugInterface debugInterface);

    void start();

    void stop();

    void sendToLisp(SlimeRequest request) throws Exception;

    void sendToLisp(SlimeRequest request, boolean startServer) throws Exception;

    void sendToLisp(SlimeRequest request, boolean startServer, Runnable onFailureServerState) throws Exception;

    String getGlobalPackage();

    SymbolState refreshSymbolFromServer(String packageName, String symbolName);

    BatchedSymbolRefreshAction refreshSymbolsFromServer();

    LispEnvironmentState getState();

    SltLispEnvironment getEnvironment();

    String macroexpand(LispList form, String packageName);

    void updateIndentation(LispElement element);

    Integer calculateOffset(PsiElement element, PsiFile file, boolean wasAfter, String text, int offset, String packageOverride);

    void addBreakpoint(XBreakpoint<SltBreakpointProperties> nativeBreakpoint);

    void removeBreakpoint(XBreakpoint<SltBreakpointProperties> nativeBreakpoint);

    void nativeBreakpointUpdated(XBreakpoint<SltBreakpointProperties> nativeBreakpoint);

    Collection<SltBreakpoint> getAllBreakpoints();

    LispSltOverrides getOverrides();

    boolean hasFeature(LispFeatures feature);

    String getBreakpointsForInstall();

    enum LispEnvironmentState {
        STOPPED, READY, INITIALIZING
    }

    interface LispEnvironmentListener extends SltLispOutputChangedListener {

        void onPreStart();
        void onPostStart();
        void onPreStop();
        void onPostStop();

    }
}

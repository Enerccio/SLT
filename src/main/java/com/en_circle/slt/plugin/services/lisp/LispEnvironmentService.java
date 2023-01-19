package com.en_circle.slt.plugin.services.lisp;

import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.environment.SltLispEnvironment;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltLispOutputChangedListener;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;


public interface LispEnvironmentService extends Disposable {

    static LispEnvironmentService getInstance(Project project) {
        LispEnvironmentServiceImpl impl = (LispEnvironmentServiceImpl) project.getService(LispEnvironmentService.class);
        boolean wasInit = impl.initProject(project);
        if (wasInit) {
            impl.postInit();
        }
        return impl;
    }

    void resetConfiguration();

    void addServerListener(LispEnvironmentListener listener);

    void setRequestResponseLogger(RequestResponseLogger logger);

    void setDebugInterface(DebugInterface debugInterface);

    void start();

    void stop();

    void sendToLisp(SlimeRequest request) throws Exception;

    void sendToLisp(SlimeRequest request, boolean startServer) throws Exception;

    String getGlobalPackage();

    SymbolState refreshSymbolFromServer(String packageName, String symbolName, PsiElement element);

    LispEnvironmentState getState();

    SltLispEnvironment getEnvironment();

    String macroexpand(LispList form, String packageName);

    void updateIndentation(LispElement element);

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

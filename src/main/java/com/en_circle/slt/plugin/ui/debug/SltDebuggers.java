package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.SltLispEnvironmentProvider.SBCLServerListener;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.debug.SltDebugInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.content.Content;
import com.intellij.ui.tabs.impl.JBTabsImpl;

import javax.swing.*;
import java.awt.*;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class SltDebuggers implements DebugInterface, SBCLServerListener {

    private final ToolWindow toolWindow;
    private final JPanel content;
    private final JBTabsImpl tabs;

    private final Map<BigInteger, SltDebugger> activeDebuggers = new HashMap<>();
    private Content self;

    public SltDebuggers(ToolWindow toolWindow) {
        this.toolWindow = toolWindow;
        this.content = new JPanel(new BorderLayout());
        this.tabs = new JBTabsImpl(toolWindow.getProject());
        this.content.add(this.tabs.getComponent());

        SltLispEnvironmentProvider.getInstance().addServerListener(this);
        SltLispEnvironmentProvider.getInstance().setDebugInterface(this);
    }

    public JPanel getContent() {
        return content;
    }

    @Override
    public void onDebugCreate(SltDebugInfo info) {
        ApplicationManager.getApplication().invokeLater(() -> {
            if (activeDebuggers.containsKey(info.getThreadId())) {
                activeDebuggers.get(info.getThreadId()).redraw(info);
            } else {
                SltDebugger debugger = new SltDebuggerImpl(this);
                debugger.redraw(info);
                activeDebuggers.put(info.getThreadId(), debugger);
                tabs.addTab(debugger.getTab());
            }
        });
    }

    @Override
    public void onDebugActivate(BigInteger debugId, BigInteger level) {
        ApplicationManager.getApplication().invokeLater(() -> {
            if (activeDebuggers.containsKey(debugId)) {
                activeDebuggers.get(debugId).activate();
                tabs.select(activeDebuggers.get(debugId).getTab(), true);
                tabs.requestFocusInWindow();
                toolWindow.getContentManager().setSelectedContent(self, true);
            }
        });
    }

    @Override
    public void onDebugReturn(BigInteger debugId, BigInteger level) {
        ApplicationManager.getApplication().invokeLater(() -> {
            if (activeDebuggers.containsKey(debugId)) {
                removeDebugger(activeDebuggers.get(debugId), debugId);
            }
        });
    }

    public void removeDebugger(SltDebugger sltDebugger, BigInteger id) {
        tabs.removeTab(sltDebugger.getTab());
        activeDebuggers.remove(id);
    }

    public void setSelf(Content self) {
        this.self = self;
    }

    @Override
    public void onPreStart() {

    }

    @Override
    public void onPostStart() {

    }

    @Override
    public void onPreStop() {

    }

    @Override
    public void onPostStop() {
        activeDebuggers.clear();
        tabs.removeAllTabs();
    }

    @Override
    public void onOutputChanged(SltOutput output, String newData) {

    }

    public Project getProject() {
        return toolWindow.getProject();
    }
}

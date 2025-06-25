package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentListener;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.debug.SltDebugInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.content.Content;
import com.intellij.ui.tabs.JBTabs;
import com.intellij.ui.tabs.JBTabsFactory;

import javax.swing.*;
import java.awt.BorderLayout;
import java.math.BigInteger;
import java.util.*;

public class SltDebuggers implements DebugInterface, LispEnvironmentListener {

    private final ToolWindow toolWindow;
    private final JPanel content;
    private final JBTabs tabs;

    private final Map<BigInteger, SltDebugger> activeDebuggers = Collections.synchronizedMap(new HashMap<>());
    private final Set<BigInteger> toActivateDebuggers = Collections.synchronizedSet(new HashSet<>());
    private Content self;

    public SltDebuggers(ToolWindow toolWindow) {
        this.toolWindow = toolWindow;
        this.content = new JPanel(new BorderLayout());
        this.tabs = JBTabsFactory.createTabs(toolWindow.getProject());
        this.content.add(this.tabs.getComponent());

        LispEnvironmentService.getInstance(toolWindow.getProject()).addServerListener(this);
        LispEnvironmentService.getInstance(toolWindow.getProject()).setDebugInterface(this);
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
                if (toActivateDebuggers.contains(info.getThreadId())) {
                    toActivateDebuggers.remove(info.getThreadId());

                    onDebugActivate(info.getThreadId(), null);
                }
            }
        });
    }

    @Override
    public void onDebugActivate(BigInteger debugId, BigInteger level) {
        ApplicationManager.getApplication().invokeLater(() -> {
            if (activeDebuggers.containsKey(debugId)) {
                activeDebuggers.get(debugId).activate();
                tabs.select(activeDebuggers.get(debugId).getTab(), true);
                ((JComponent) tabs).requestFocusInWindow();
                toolWindow.getContentManager().setSelectedContent(self, true);
            } else {
                toActivateDebuggers.add(debugId);
            }
        });
    }

    @Override
    public void onDebugReturn(BigInteger debugId, BigInteger level) {
        ApplicationManager.getApplication().invokeLater(() -> {
            if (activeDebuggers.containsKey(debugId)) {
                removeDebugger(activeDebuggers.get(debugId), debugId);
            } else {
                toActivateDebuggers.remove(debugId);
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

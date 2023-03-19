package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.services.SltProjectService;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.components.JBTextField;
import com.intellij.ui.jcef.JBCefBrowser;
import com.intellij.ui.jcef.JBCefClient;
import org.cef.browser.CefBrowser;
import org.cef.handler.CefLoadHandlerAdapter;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ContainerAdapter;
import java.awt.event.ContainerEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class SltHyperspecView implements Disposable {

    private static final String BASE_URL = "https://www.lispworks.com/documentation/HyperSpec/";

    private final ToolWindow toolWindow;
    private final Project project;
    private final JPanel panel;
    private final JBCefBrowser browser;
    private final JBCefClient client;
    private final JBTextField address;
    private boolean hidden = true;

    private boolean loading = false;

    public SltHyperspecView(ToolWindow toolWindow) {
        this.toolWindow = toolWindow;
        this.project = toolWindow.getProject();

        panel = new JPanel(new BorderLayout());
        panel.addContainerListener(new ContainerAdapter() {
            @Override
            public void componentAdded(ContainerEvent e) {
                if (hidden) {
                    showRoot();
                }
                hidden = false;
            }

            @Override
            public void componentRemoved(ContainerEvent e) {
                hidden = true;
            }
        });
        browser = new JBCefBrowser();
        client = browser.getJBCefClient();
        client.addLoadHandler(new CefLoadHandlerAdapter() {
            @Override
            public void onLoadingStateChange(CefBrowser browser, boolean isLoading, boolean canGoBack, boolean canGoForward) {
                loading = isLoading;
                address.setText(browser.getURL());
            }
        }, browser.getCefBrowser());

        panel.add(browser.getComponent(), BorderLayout.CENTER);

        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new GoBackAction());
        controlGroup.add(new GoForwardAction());
        controlGroup.addSeparator();
        controlGroup.add(new StopAction());
        controlGroup.add(new RefreshAction());
        controlGroup.addSeparator();
        controlGroup.add(new HomeAction());

        JPanel header = new JPanel(new BorderLayout());
        ActionToolbar toolbar = ActionManager.getInstance()
                .createActionToolbar("SltHyperspecToolbar", controlGroup, true);
        toolbar.setTargetComponent(header);
        header.add(toolbar.getComponent(), BorderLayout.NORTH);

        address = new JBTextField();
        address.setEditable(false);
        address.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                address.setSelectionStart(0);
                address.setSelectionEnd(address.getText().length());
            }
        });
        header.add(address, BorderLayout.SOUTH);

        panel.add(header, BorderLayout.NORTH);

        SltProjectService.getInstance(project).setHyperspecView(this);
    }

    @Override
    public void dispose() {
        browser.dispose();
    }

    public JComponent getContent() {
        return panel;
    }

    private void showRoot() {
        browser.loadURL(BASE_URL);
    }

    private void stop() {
        browser.getCefBrowser().stopLoad();
    }

    private void refresh() {
        browser.getCefBrowser().reloadIgnoreCache();
    }

    private void goBack() {
        browser.getCefBrowser().goBack();
    }

    private void goForward() {
        browser.getCefBrowser().goForward();
    }

    public void showUrl(String url) {
        browser.loadURL(url);
    }

    private class GoBackAction extends AnAction {

        private GoBackAction() {
            super(SltBundle.message("slt.ui.clhs.back"), "", AllIcons.Actions.Back);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            goBack();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(browser.getCefBrowser().canGoBack());
        }
    }

    private class GoForwardAction extends AnAction {

        private GoForwardAction() {
            super(SltBundle.message("slt.ui.clhs.forward"), "", AllIcons.Actions.Forward);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            goForward();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(browser.getCefBrowser().canGoForward());
        }
    }

    private class StopAction extends AnAction {

        private StopAction() {
            super(SltBundle.message("slt.ui.clhs.stop"), "", AllIcons.Process.Stop);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            stop();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(loading);
        }
    }

    private class RefreshAction extends AnAction {

        private RefreshAction() {
            super(SltBundle.message("slt.ui.clhs.refresh"), "", AllIcons.Actions.Refresh);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            refresh();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(!loading);
        }
    }

    private class HomeAction extends AnAction {

        private HomeAction() {
            super(SltBundle.message("slt.ui.clhs.home"), "", AllIcons.Nodes.HomeFolder);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            showRoot();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(true);
        }
    }
}

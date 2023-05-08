package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltIconProvider;
import com.en_circle.slt.plugin.services.SltProjectService;
import com.intellij.icons.AllIcons;
import com.intellij.icons.AllIcons.General;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.components.JBTextField;
import com.intellij.ui.content.Content;
import com.intellij.ui.jcef.JBCefBrowser;
import com.intellij.ui.jcef.JBCefClient;
import org.cef.browser.CefBrowser;
import org.cef.browser.CefFrame;
import org.cef.handler.CefLoadHandlerAdapter;
import org.cef.handler.CefRequestHandlerAdapter;
import org.cef.handler.CefResourceRequestHandler;
import org.cef.misc.BoolRef;
import org.cef.network.CefRequest;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ContainerAdapter;
import java.awt.event.ContainerEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;

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
    private OsThemeMode themeMode = OsThemeMode.OS;
    private Content self;

    public SltHyperspecView(ToolWindow toolWindow) {
        this.toolWindow = toolWindow;
        this.project = toolWindow.getProject();

        panel = new JPanel(new BorderLayout());
        panel.addContainerListener(new ContainerAdapter() {
            @Override
            public void componentAdded(ContainerEvent e) {
                if (hidden) {
                    ApplicationManager.getApplication().invokeLater(() -> showRoot());
                }
                hidden = false;
            }

            @Override
            public void componentRemoved(ContainerEvent e) {
                hidden = true;
            }
        });
        browser = new JBCefBrowser();
        browser.getCefBrowser().getDevTools();
        client = browser.getJBCefClient();
        client.addLoadHandler(new CefLoadHandlerAdapter() {
            @Override
            public void onLoadingStateChange(CefBrowser browser, boolean isLoading, boolean canGoBack, boolean canGoForward) {
                loading = isLoading;
                address.setText(browser.getURL());
            }
        }, browser.getCefBrowser());
        client.addRequestHandler(new CefRequestHandlerAdapter() {
            @Override
            public CefResourceRequestHandler getResourceRequestHandler(CefBrowser browser, CefFrame frame, CefRequest request, boolean isNavigation, boolean isDownload, String requestInitiator, BoolRef disableDefaultHandling) {
                if (themeMode != OsThemeMode.OS) {
                    HashMap<String, String> headerMap = new HashMap<>();
                    request.getHeaderMap(headerMap);
                    headerMap.remove("Sec-CH-Prefers-Color-Scheme");
                    if (themeMode == OsThemeMode.DARK) {
                        headerMap.put("Sec-CH-Prefers-Color-Scheme", "dark");
                    } else {
                        headerMap.put("Sec-CH-Prefers-Color-Scheme", "light");
                    }
                    request.setHeaderMap(headerMap);
                }
                return null;
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
        controlGroup.addSeparator();
        controlGroup.add(new ThemeAction());
        controlGroup.add(new ResetZoom());
        controlGroup.add(new ZoomIn());
        controlGroup.add(new ZoomOut());

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

    public void setSelf(Content self) {
        this.self = self;
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

    public void showContent() {
        toolWindow.getContentManager().setSelectedContent(self, true);
    }

    private void reload() {
        browser.getCefBrowser().reloadIgnoreCache();
    }

    private void resetZoom() {
        browser.setZoomLevel(1.0);
    }

    private void zoomIn() {
        browser.setZoomLevel(Math.min(browser.getZoomLevel() + 0.15, 5f));
    }

    private void zoomOut() {
        browser.setZoomLevel(Math.max(browser.getZoomLevel() - 0.15, 0.1));
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

    private class ThemeAction extends AnAction {

        private ThemeAction() {
            super(SltBundle.message("slt.ui.clhs.theme"), "", SltIconProvider.systemTheme);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            themeMode = themeMode.next();
            reload();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(true);
            switch (themeMode) {
                case DARK -> e.getPresentation().setIcon(SltIconProvider.darkTheme);
                case LIGHT -> e.getPresentation().setIcon(SltIconProvider.lightTheme);
                case OS -> e.getPresentation().setIcon(SltIconProvider.systemTheme);
            }
        }
    }

    private enum OsThemeMode {
        DARK, LIGHT, OS;

        public OsThemeMode next() {
            return switch (this) {
                case DARK -> LIGHT;
                case LIGHT -> OS;
                case OS -> DARK;
            };
        }
    }

    private class ResetZoom extends AnAction {

        private ResetZoom() {
            super(SltBundle.message("slt.ui.clhs.zoom.reset"), "", General.ActualZoom);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            resetZoom();
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

    private class ZoomIn extends AnAction {

        private ZoomIn() {
            super(SltBundle.message("slt.ui.clhs.zoom.zoomin"), "", General.ZoomIn);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            zoomIn();
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

    private class ZoomOut extends AnAction {

        private ZoomOut() {
            super(SltBundle.message("slt.ui.clhs.zoom.zoomout"), "", General.ZoomOut);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            zoomOut();
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

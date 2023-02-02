package com.en_circle.slt.plugin.ui.instance;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.plugin.swank.components.ThreadInfo;
import com.en_circle.slt.plugin.swank.requests.KillThread;
import com.en_circle.slt.plugin.swank.requests.ListThreads;
import com.en_circle.slt.plugin.swank.requests.SuspendThread;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.table.JBTable;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableModel;
import java.awt.*;
import java.math.BigInteger;

public class ThreadList implements InstanceInfoPanelComponent {
    private static final Logger log = LoggerFactory.getLogger(ThreadList.class);

    private Project project;
    private JPanel content;
    private JPanel tableContainer;
    private JBTable table = null;
    private ThreadInfo info;

    @Override
    public Component create(Project project) {
        this.project = project;
        content = new JPanel(new BorderLayout());

        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new RefreshLispThreads());
        controlGroup.add(new BreakThread());
        controlGroup.add(new TerminateThread());
        JPanel west = new JPanel(new BorderLayout());
        ActionToolbar toolbar = ActionManager.getInstance()
                .createActionToolbar("ThreadList", controlGroup, false);
        toolbar.setTargetComponent(content);
        west.add(toolbar.getComponent(), BorderLayout.NORTH);
        content.add(west, BorderLayout.WEST);

        tableContainer = new JPanel(new BorderLayout());
        tableContainer.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(),
                SltBundle.message("slt.ui.instanceinfo.threads.title")));
        content.add(tableContainer, BorderLayout.CENTER);

        table = new JBTable();
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.setColumnSelectionAllowed(false);
        tableContainer.add(new JBScrollPane(table), BorderLayout.CENTER);
        tableContainer.add(table.getTableHeader(), BorderLayout.NORTH);

        return content;
    }

    @Override
    public Component getComponent() {
        return content;
    }

    private void refreshThreads() {
        try {
            LispEnvironmentService.getInstance(project)
                    .sendToLisp(ListThreads.dumpThreads((info) ->
                            ApplicationManager.getApplication().invokeLater(() -> regenerateTable(info))), false);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.start"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
        }
    }

    private void regenerateTable(ThreadInfo info) {
        this.info = info;
        table.setModel(new TableModel() {

            @Override
            public int getRowCount() {
                return info.getRowCount();
            }

            @Override
            public int getColumnCount() {
                return info.getMaxHeaders();
            }

            @Nls
            @Override
            public String getColumnName(int columnIndex) {
                return info.getHeader(columnIndex);
            }

            @Override
            public Class<?> getColumnClass(int columnIndex) {
                return String.class;
            }

            @Override
            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return false;
            }

            @Override
            public Object getValueAt(int rowIndex, int columnIndex) {
                return info.getDataString(rowIndex, columnIndex);
            }

            @Override
            public void setValueAt(Object aValue, int rowIndex, int columnIndex) {

            }

            @Override
            public void addTableModelListener(TableModelListener l) {

            }

            @Override
            public void removeTableModelListener(TableModelListener l) {

            }
        });
    }

    private void pauseThread() {
        int row = table.getSelectionModel().getSelectedIndices()[0];
        BigInteger lispId = BigInteger.valueOf(row);
        try {
            LispEnvironmentService.getInstance(project)
                    .sendToLisp(SuspendThread.suspend(lispId,
                            () -> ApplicationManager.getApplication().invokeLater(this::refreshThreads)), false);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.start"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
        }
    }

    private void terminateThread() {
        int row = table.getSelectionModel().getSelectedIndices()[0];
        BigInteger lispId = BigInteger.valueOf(row);
        try {
            LispEnvironmentService.getInstance(project)
                    .sendToLisp(KillThread.kill(lispId,
                            () -> ApplicationManager.getApplication().invokeLater(this::refreshThreads)), false);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.start"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
        }
    }

    @Override
    public void onOutputChanged(SltOutput output, String newData) {

    }

    @Override
    public void onPreStart() {

    }

    @Override
    public void onPostStart() {
        refreshThreads();
    }

    @Override
    public void onPreStop() {
        tableContainer.removeAll();
    }

    @Override
    public void onPostStop() {

    }

    private class RefreshLispThreads extends AnAction {

        private RefreshLispThreads() {
            super(SltBundle.message("slt.ui.instanceinfo.threads.action.refresh"), "", Actions.Refresh);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            refreshThreads();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(LispEnvironmentService.getInstance(project).getState() == LispEnvironmentState.READY);
        }
    }

    private class BreakThread extends AnAction {

        private BreakThread() {
            super(SltBundle.message("slt.ui.instanceinfo.threads.action.break"), "", Actions.Pause);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            pauseThread();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(LispEnvironmentService.getInstance(project).getState() == LispEnvironmentState.READY
                && !table.getSelectionModel().isSelectionEmpty());
        }
    }

    private class TerminateThread extends AnAction {

        private TerminateThread() {
            super(SltBundle.message("slt.ui.instanceinfo.threads.action.stop"), "", Actions.Suspend);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            terminateThread();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(LispEnvironmentService.getInstance(project).getState() == LispEnvironmentState.READY
                    && !table.getSelectionModel().isSelectionEmpty());
        }
    }

}

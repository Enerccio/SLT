package com.en_circle.slt.plugin.ui.sdk;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.environment.Environment;
import com.en_circle.slt.plugin.sdk.LispSdk;
import com.en_circle.slt.plugin.sdk.SdkList;
import com.en_circle.slt.tools.platform.PlatformAction;
import com.en_circle.slt.tools.platform.PlatformActionsContainer;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.icons.AllIcons.General;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.ui.SelectFromListDialog;
import com.intellij.openapi.util.NlsContexts.ConfigurableName;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.table.JBTable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.util.List;
import java.util.*;

public class SdkConfigurer implements Configurable {

    private JPanel root;
    private JBTable table;
    private final List<LispSdk> definedSdks = new ArrayList<>();

    @Override
    public @ConfigurableName String getDisplayName() {
        return SltBundle.message("slt.ui.settings.sdk");
    }

    @Override
    public @Nullable JComponent createComponent() {
        root = new JPanel(new BorderLayout());
        table = new JBTable(new SdkListModel(Collections.emptyList()));
        JBScrollPane scrollPane = new JBScrollPane(table);
        root.add(scrollPane, BorderLayout.CENTER);

        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new EditSdkAction());
        controlGroup.add(new AddSdkAction());
        controlGroup.add(new DownloadSdkAction());
        controlGroup.add(new RemoveSdkAction());
        JPanel east = new JPanel(new BorderLayout());
        ActionToolbar toolbar = ActionManager.getInstance()
                .createActionToolbar("SltSDKsConfiguration", controlGroup, false);
        toolbar.setTargetComponent(root);
        east.add(toolbar.getComponent(), BorderLayout.NORTH);
        root.add(east, BorderLayout.EAST);

        return root;
    }

    @Override
    public boolean isModified() {
        SdkList list = SdkList.getInstance();
        if (list.sdks.size() != definedSdks.size()) {
            return true;
        }
        for (int i=0; i<definedSdks.size(); i++) {
            LispSdk sdkOrig = list.sdks.get(i);
            LispSdk sdkNew = definedSdks.get(i);
            if (!sdkNew.equals(sdkOrig)) {
                return true;
            }
        }

        return false;
    }

    @Override
    public void apply() throws ConfigurationException {
        SdkList list = SdkList.getInstance();
        list.sdks.clear();
        list.sdks.addAll(definedSdks);
    }

    @Override
    public void reset() {
        definedSdks.clear();
        SdkList list = SdkList.getInstance();
        for (LispSdk sdk : list.sdks) {
            LispSdk copy = new LispSdk();
            copy.loadState(sdk);
            definedSdks.add(copy);
        }

        SdkListModel model = new SdkListModel(definedSdks);
        table.setModel(model);
    }

    private void addSdk() {
        LispSdk newSdk = new LispSdk();
        newSdk.uuid = UUID.randomUUID().toString();


        Environment environment = selectEnvironment();
        if (environment == null) {
            return;
        }

        DialogWrapper configuration = environment.getDefinition().getDialogProvider().createSdkConfiguration(
                root, newSdk, SltBundle.message("slt.ui.settings.sdk.editor.title.new"),
                sdk -> {
                    sdk.environment = environment;
                    definedSdks.add(sdk);
                    SdkListModel model = new SdkListModel(definedSdks);
                    table.setModel(model);
        });
        configuration.show();
    }

    private void editSdk() {
        LispSdk sdk = definedSdks.get(table.getSelectedRow());
        LispSdk copy = new LispSdk();
        copy.loadState(sdk);

        Environment environment = copy.environment;
        DialogWrapper configuration = environment.getDefinition().getDialogProvider().createSdkConfiguration(
                root, copy, SltBundle.message("slt.ui.settings.sdk.editor.title.edit"),
                sdkModified -> {
                    sdkModified.environment = environment;
                    int ix = definedSdks.indexOf(sdk);
                    definedSdks.remove(ix);
                    definedSdks.add(ix, sdkModified);
                    SdkListModel model = new SdkListModel(definedSdks);
                    table.setModel(model);
        });
        configuration.show();
    }

    private void removeSdk() {
        LispSdk sdk = definedSdks.get(table.getSelectedRow());
        table.getSelectionModel().clearSelection();
        definedSdks.remove(sdk);
        SdkListModel model = new SdkListModel(definedSdks);
        table.setModel(model);
    }

    @SuppressWarnings("IncorrectParentDisposable")
    private void downloadSdk() {
        List<Environment> downloadable = new ArrayList<>();
        for (Environment environment : Environment.values()) {
            if (environment.getDefinition().getDownloadActionDef() != null) {
                downloadable.add(environment);
            }
        }

        Environment environment = selectEnvironment(downloadable.toArray(new Environment[0]));
        if (environment == null) {
            return;
        }

        Objects.requireNonNull(PlatformActionsContainer.getAction(environment.getDefinition().getDownloadActionDef()))
                .downloadSdk(ApplicationManager.getApplication(), root,
                sdk -> {
            if (sdk != null) {
                sdk.environment = environment;
                definedSdks.add(sdk);
                SdkListModel model = new SdkListModel(definedSdks);
                table.setModel(model);
            } else {
                Messages.showErrorDialog(SltBundle.message("slt.ui.settings.sdk.download.failed"),
                        SltBundle.message("slt.ui.settings.sdk.download.failed.title"));
            }
        });
    }

    private Environment selectEnvironment() {
        return selectEnvironment(Environment.values());
    }

    private Environment selectEnvironment(Environment[] environments) {

        SelectFromListDialog selectFromListDialog = new SelectFromListDialog(null,
                environments, e -> ((Environment)e).getDefinition().getName(),
                SltBundle.message("slt.ui.settings.sdk.select"),
                ListSelectionModel.SINGLE_SELECTION);
        selectFromListDialog.setSize(450, 250);
        if (selectFromListDialog.showAndGet()) {
            Object[] selection = selectFromListDialog.getSelection();
            if (selection.length > 0) {
                return (Environment) selection[0];
            }
        }
        return null;
    }

    private class EditSdkAction extends AnAction {

        public EditSdkAction() {
            super(SltBundle.message("slt.ui.settings.sdk.edit"), "", Actions.Edit);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            editSdk();
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            e.getPresentation().setEnabled(table.getSelectionModel() != null && !table.getSelectionModel().isSelectionEmpty());
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

    }

    private class AddSdkAction extends AnAction {

        public AddSdkAction() {
            super(SltBundle.message("slt.ui.settings.sdk.add"), "", General.AddJdk);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            addSdk();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

    }

    private class DownloadSdkAction extends AnAction {

        public DownloadSdkAction() {
            super(SltBundle.message("slt.ui.settings.sdk.download"), "", Actions.Download);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            downloadSdk();
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            boolean enable = false;
            for (Environment environment : Environment.values()) {
                Class<? extends PlatformAction> actionDef = environment.getDefinition().getDownloadActionDef();
                if (actionDef != null) {
                    if (PlatformActionsContainer.hasAction(actionDef)) {
                        enable = true;
                        break;
                    }
                }
            }

            e.getPresentation().setEnabled(enable);
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

    }

    private class RemoveSdkAction extends AnAction {

        public RemoveSdkAction() {
            super(SltBundle.message("slt.ui.settings.sdk.remove"), "", General.Remove);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            removeSdk();
        }

        @Override
        public @NotNull ActionUpdateThread getActionUpdateThread() {
            return ActionUpdateThread.EDT;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            e.getPresentation().setEnabled(table.getSelectionModel() != null && !table.getSelectionModel().isSelectionEmpty());
        }

    }

    private static class SdkListModel extends AbstractTableModel {

        public final List<LispSdk> items = new ArrayList<>();

        public SdkListModel(List<LispSdk> sdks) {
            this.items.addAll(sdks);
        }

        @Override
        public int getRowCount() {
            return items.size();
        }

        @Override
        public int getColumnCount() {
            return 3;
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            return switch (columnIndex) {
                case 0 -> items.get(rowIndex).userName;
                case 1 -> items.get(rowIndex).getEnvironment().getDefinition().getName();
                case 2 -> items.get(rowIndex).quickLispPath;
                default -> null;
            };
        }

        @Override
        public String getColumnName(int column) {
            return switch (column) {
                case 0 -> SltBundle.message("slt.ui.settings.sdk.table.column.name");
                case 1 -> SltBundle.message("slt.ui.settings.sdk.table.column.type");
                case 2 -> SltBundle.message("slt.ui.settings.sdk.table.column.quicklisp");
                default -> null;
            };
        }
    }

}

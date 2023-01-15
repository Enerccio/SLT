package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.swank.requests.EvalAndGrab;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.actionSystem.ex.CustomComponentAction;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.ui.ComboBox;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.components.JBLabel;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

public class PackageSelectorComponent {
    private static final Logger log = LoggerFactory.getLogger(PackageSelectorComponent.class);

    private final ActionToolbar content;
    private final Supplier<String> currentPackage;
    private final ComboBox<String> packageComboBox;
    private PackageChangedListener listener;

    public PackageSelectorComponent(String id, Supplier<String> currentPackage) {
        this.currentPackage = currentPackage;
        this.packageComboBox = new ComboBox<>();
        this.packageComboBox.addActionListener(e -> {
            String item = (String) packageComboBox.getSelectedItem();
            if (listener != null) {
                listener.onPackageChange(item);
            }
        });

        DefaultActionGroup controlGroup = new DefaultActionGroup();
        controlGroup.add(new ComboBoxSelectorAction());
        controlGroup.add(new RefreshPackagesAction());
        this.content = ActionManager.getInstance()
                .createActionToolbar(id, controlGroup, true);
    }

    public ActionToolbar getActionToolbar() {
        return content;
    }

    public void refresh() {
        try {
            SltLispEnvironmentProvider.getInstance().sendToLisp(EvalAndGrab.eval("(slt-core:list-package-names)", true, (result, stdout, parsed) -> {
                resolvePackages(parsed);
            }), false);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.sbclstart"), e);
            Messages.showErrorDialog(ProjectManager.getInstance().getDefaultProject(),
                    e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
        }
    }

    private void resolvePackages(List<LispElement> parsed) {
        List<String> packages = new ArrayList<>();
        if (parsed.size() == 1) {
            LispContainer container = (LispContainer) parsed.get(0);
            for (LispElement element : container.getItems()) {
                if (element instanceof LispString) {
                    packages.add(((LispString) element).getValue());
                }
            }
        }
        if (packages.contains("COMMON-LISP-USER")) {
            packages.add("CL-USER");
        }
        packages.sort(String::compareToIgnoreCase);
        packageComboBox.setModel(new DefaultComboBoxModel<>(packages.toArray(new String[0])));
        if (packages.contains(currentPackage.get())) {
            packageComboBox.setSelectedItem(currentPackage.get());
        } else {
            packageComboBox.setSelectedItem("CL-USER");
        }
    }

    public void setListener(PackageChangedListener listener) {
        this.listener = listener;
    }

    public JComponent getComponent() {
        return content.getComponent();
    }

    private class ComboBoxSelectorAction extends AnAction implements CustomComponentAction {

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {

        }

        @Override
        public @NotNull JComponent createCustomComponent(@NotNull Presentation presentation, @NotNull String place) {
            JPanel panel = new JPanel(new BorderLayout());
            panel.add(packageComboBox, BorderLayout.CENTER);
            panel.add(new JBLabel(SltBundle.message("slt.ui.packageselector.title")), BorderLayout.WEST);
            return panel;
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(SltLispEnvironmentProvider.getInstance().isLispEnvironmentActive());
        }

    }

    private class RefreshPackagesAction extends AnAction {

        private RefreshPackagesAction() {
            super(SltBundle.message("slt.ui.packageselector.refresh"), "", Actions.Refresh);
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
            refresh();
        }

        @Override
        public void update(@NotNull AnActionEvent e) {
            super.update(e);

            e.getPresentation().setEnabled(SltLispEnvironmentProvider.getInstance().isLispEnvironmentActive());
        }
    }

    public interface PackageChangedListener {

        void onPackageChange(String packageName);

    }

}

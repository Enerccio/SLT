package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.swank.requests.SltFrameLocalsAndCatchTags;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.table.JBTable;
import com.intellij.ui.tabs.JBTabs;
import com.intellij.ui.tabs.TabInfo;
import com.intellij.ui.tabs.impl.JBTabsImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class SltFrameInfo {
    private static final Logger log = LoggerFactory.getLogger(SltFrameInfo.class);

    private final Project project;
    private final BigInteger threadId;
    private final BigInteger frameId;
    private final String module;
    private final JComponent content;
    private JBTable localsTable;

    public SltFrameInfo(Project project, BigInteger threadId, BigInteger frameId, String module) {
        this.project = project;
        this.threadId = threadId;
        this.frameId = frameId;
        this.module = module;

        content = new JPanel(new BorderLayout());

        create();
    }

    private void create() {
        JBTabs tabs = new JBTabsImpl(project);

        localsTable = new JBTable(new FrameTableModel(new ArrayList<>()));
        localsTable.setFillsViewportHeight(true);

        TabInfo locals = new TabInfo(new JBScrollPane(localsTable));
        locals.setText(SltBundle.message("slt.ui.debugger.frame.locals"));
        tabs.addTab(locals);

        SltFrameConsole frameConsole = new SltFrameConsole(project, threadId, frameId, this::reloadLocals, module);
        TabInfo consoleTab = frameConsole.create();
        tabs.addTab(consoleTab);
        content.add(tabs.getComponent(), BorderLayout.CENTER);
    }

    private void reloadLocals() {
        try {
            SltLispEnvironmentProvider.getInstance().sendToLisp(SltFrameLocalsAndCatchTags.getLocals(frameId, threadId, result -> {
                ApplicationManager.getApplication().invokeLater(() -> {
                    refreshFrameValues(result);
                });
            }), false);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.sbclstart"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
        }
    }

    public JComponent getContent() {
        return content;
    }

    public void refreshFrameValues(LispElement localsAndTags) {
        LispElement locals = ((LispContainer) localsAndTags).getItems().get(0);
        if (locals instanceof LispContainer) {
            LispContainer locs = (LispContainer) locals;
            List<Local> parsedLocals = new ArrayList<>();
            for (LispElement e : locs.getItems()) {
                try {
                    LispContainer ec = (LispContainer) e;
                    String name = ((LispString) LispUtils.pvalue(ec, new LispSymbol(":NAME"))).getValue();
                    String value = ((LispString) LispUtils.pvalue(ec, new LispSymbol(":VALUE"))).getValue();
                    Local l = new Local();
                    l.name = name;
                    l.value = value;
                    parsedLocals.add(l);
                } catch (Exception ignored) {
                    Local l = new Local();
                    l.name = SltBundle.message("slt.ui.debugger.frame.failedtoparse");
                    l.value = SltBundle.message("slt.ui.debugger.frame.failedtoparse");
                    parsedLocals.add(l);
                }
            }
            localsTable.setModel(new FrameTableModel(parsedLocals));
        } else {
            localsTable.setModel(new FrameTableModel(new ArrayList<>()));
        }
    }

    private static class Local {

        private String name;
        private String value;

    }

    private static class FrameTableModel extends AbstractTableModel {

        private final List<Local> locals;

        private FrameTableModel(List<Local> locals) {
            this.locals = locals;
        }

        @Override
        public String getColumnName(int column) {
            switch (column) {
                case 0: return SltBundle.message("slt.ui.debugger.frame.arg");
                case 1: return SltBundle.message("slt.ui.debugger.frame.value");
            }
            return null;
        }

        @Override
        public int getRowCount() {
            return locals.size();
        }

        @Override
        public int getColumnCount() {
            return 2;
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            if (columnIndex == 0) {
                return locals.get(rowIndex).name;
            } else {
                return locals.get(rowIndex).value;
            }
        }
    }
}

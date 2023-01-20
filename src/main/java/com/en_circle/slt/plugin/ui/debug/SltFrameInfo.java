package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltUIConstants;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.requests.FrameLocalsAndCatchTags;
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
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SltFrameInfo {
    private static final Logger log = LoggerFactory.getLogger(SltFrameInfo.class);

    private final Project project;
    private final BigInteger threadId;
    private final BigInteger frameId;
    private final String module;
    private final JComponent content;
    private SltInspector inspector;
    private JBTable localsTable;
    private JBTabs tabs;
    private TabInfo inspectorTab;

    public SltFrameInfo(Project project, BigInteger threadId, BigInteger frameId, String module) {
        this.project = project;
        this.threadId = threadId;
        this.frameId = frameId;
        this.module = module;

        content = new JPanel(new BorderLayout());

        create();
    }

    private void create() {
        tabs = new JBTabsImpl(project);

        localsTable = new JBTable(new FrameTableModel(new ArrayList<>()));
        localsTable.setFillsViewportHeight(true);
        localsTable.setFocusable(false);
        localsTable.setRowSelectionAllowed(false);
        localsTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                int row = localsTable.rowAtPoint(e.getPoint());
                if (row >= 0) {
                    int column = localsTable.columnAtPoint(e.getPoint());
                    if (column == 0) {
                        FrameTableModel model = (FrameTableModel) localsTable.getModel();
                        openInspector(model.locals.get(row));
                    }
                }
            }
        });
        resetRenderer();

        TabInfo locals = new TabInfo(new JBScrollPane(localsTable));
        locals.setText(SltBundle.message("slt.ui.debugger.frame.locals"));
        tabs.addTab(locals);

        SltFrameConsole frameConsole = new SltFrameConsole(project, threadId, frameId, this::reloadLocals, module);
        TabInfo consoleTab = frameConsole.create();
        tabs.addTab(consoleTab);

        inspector = new SltInspector(project, threadId);
        inspectorTab = new TabInfo(new JBScrollPane(inspector.getContent()));
        inspectorTab.setText(SltBundle.message("slt.ui.debugger.frame.inspector"));
        tabs.addTab(inspectorTab);

        content.add(tabs.getComponent(), BorderLayout.CENTER);
    }

    private void resetRenderer() {
        localsTable.getColumn(SltBundle.message("slt.ui.debugger.frame.arg")).setCellRenderer(new DefaultTableCellRenderer() {
            @Override
            public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

                Map<TextAttribute, Object> attributes = new HashMap<>(getFont().getAttributes());
                attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
                setFont(getFont().deriveFont(attributes));
                setForeground(SltUIConstants.HYPERLINK_COLOR);
                setCursor(new Cursor(Cursor.HAND_CURSOR));

                return this;
            }
        });
    }

    private void openInspector(Local local) {
        inspector.loadLocal(local, frameId);
        tabs.select(inspectorTab, true);
    }

    private void reloadLocals() {
        try {
            LispEnvironmentService.getInstance(project).sendToLisp(FrameLocalsAndCatchTags.getLocals(frameId, threadId, result -> {
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
        if (locals instanceof LispContainer locs) {
            List<Local> parsedLocals = new ArrayList<>();
            int ix=0;
            for (LispElement e : locs.getItems()) {
                try {
                    LispContainer ec = (LispContainer) e;
                    String name = ((LispString) LispUtils.pvalue(ec, new LispSymbol(":NAME"))).getValue();
                    String value = ((LispString) LispUtils.pvalue(ec, new LispSymbol(":VALUE"))).getValue();
                    Local l = new Local();
                    l.ix = ix++;
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
            resetRenderer();
        } else {
            localsTable.setModel(new FrameTableModel(new ArrayList<>()));
            resetRenderer();
        }
    }

    public static class Local {

        int ix;
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
            return switch (column) {
                case 0 -> SltBundle.message("slt.ui.debugger.frame.arg");
                case 1 -> SltBundle.message("slt.ui.debugger.frame.value");
                default -> null;
            };
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

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            return String.class;
        }
    }
}

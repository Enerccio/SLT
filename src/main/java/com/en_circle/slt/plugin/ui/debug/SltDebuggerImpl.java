package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.swank.debug.SltDebugAction;
import com.en_circle.slt.plugin.swank.debug.SltDebugArgument;
import com.en_circle.slt.plugin.swank.debug.SltDebugInfo;
import com.en_circle.slt.plugin.swank.requests.SltInvokeNthRestart;
import com.en_circle.slt.plugin.swank.requests.ThrowToToplevel;
import com.intellij.icons.AllIcons.Actions;
import com.intellij.openapi.actionSystem.ActionPlaces;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.JBSplitter;
import com.intellij.ui.ScrollPaneFactory;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextField;
import com.intellij.ui.table.JBTable;
import com.intellij.ui.tabs.TabInfo;
import org.codehaus.plexus.util.StringUtils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class SltDebuggerImpl implements SltDebugger {

    private final JComponent content;
    private final TabInfo tabInfo;
    private final SltDebuggers parent;
    private BigInteger lastDebugId;

    public SltDebuggerImpl(SltDebuggers parent) {
        this.parent = parent;
        this.content = new JPanel(new BorderLayout());
        this.tabInfo = new TabInfo(this.content);
        this.tabInfo.setText("DEBUGGER #");
        this.tabInfo.setBlinkCount(5);
        this.tabInfo.setTabLabelActions(new DefaultActionGroup(new AnAction("Close", "", Actions.Close) {
            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                close();
            }

        }), ActionPlaces.EDITOR_TAB);
    }

    @Override
    public TabInfo getTab() {
        return tabInfo;
    }

    @Override
    public void redraw(SltDebugInfo debugInfo) {
        this.tabInfo.setText("DEBUGGER #" + debugInfo.getThreadId());

        this.content.removeAll();
        this.lastDebugId = debugInfo.getThreadId();

        JBSplitter splitter = new JBSplitter(false);
        splitter.setProportion(0.2f);
        this.content.add(splitter, BorderLayout.CENTER);

        JPanel infoAreaPanel = new JPanel(new BorderLayout());
        infoAreaPanel.setBorder(BorderFactory.createTitledBorder("Error Message"));

        JTextArea infoArea = new JTextArea();
        infoAreaPanel.add(infoArea, BorderLayout.CENTER);
        infoArea.setEditable(false);
        infoArea.setWrapStyleWord(true);
        infoArea.setText(debugInfo.getInfo());

        JPanel conditionAreaPanel = new JPanel(new BorderLayout());
        conditionAreaPanel.setBorder(BorderFactory.createTitledBorder("Condition Detail"));
        JTextArea conditionArea = new JTextArea();
        conditionAreaPanel.add(conditionArea, BorderLayout.CENTER);
        conditionArea.setEditable(false);
        conditionArea.setWrapStyleWord(true);
        conditionArea.setText(debugInfo.getConditionInfo());

        JPanel infoPanel = new JPanel();
        infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.Y_AXIS));
        infoPanel.add(infoAreaPanel);
        infoPanel.add(conditionAreaPanel);
        splitter.setFirstComponent(new JScrollPane(infoPanel));

        JBSplitter splitter2 = new JBSplitter(false);
        splitter2.setProportion(0.4f);
        splitter.setSecondComponent(splitter2);

        JPanel actionsPanel = new JPanel();
        actionsPanel.setLayout(new BoxLayout(actionsPanel, BoxLayout.Y_AXIS));

        for (SltDebugAction action : debugInfo.getActions()) {
            JLabel label = new JLabel(getText(action.getActionName()));
            label.setCursor(new Cursor(Cursor.HAND_CURSOR));
            label.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    actionAccepted(action, debugInfo);
                }
            });
            JPanel labelPanel = new JPanel(new BorderLayout());
            labelPanel.add(label, BorderLayout.CENTER);

            JBTextField textField = new JBTextField(getText(action.getActionDescription()));
            textField.setEditable(false);

            JPanel actionInfo = new JPanel();
            actionInfo.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "Action"));
            actionInfo.setLayout(new BoxLayout(actionInfo, BoxLayout.Y_AXIS));

            actionInfo.add(labelPanel);
            actionInfo.add(textField);

            actionsPanel.add(actionInfo);
        }

        JBScrollPane pane = new JBScrollPane(actionsPanel);
        JPanel actionsPanelDecorator = new JPanel(new BorderLayout());
        actionsPanelDecorator.setBorder(BorderFactory.createTitledBorder("Actions"));
        actionsPanelDecorator.add(pane, BorderLayout.CENTER);
        splitter2.setFirstComponent(actionsPanelDecorator);

        JBTable table = new JBTable();
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.setModel(new SltDebuggerStackTraceTableModel(debugInfo));
        table.setDefaultRenderer(String.class, (jTable, o, b, b1, i, i1) -> {
            JLabel label = new JLabel((String) o);
            label.setCursor(new Cursor(Cursor.HAND_CURSOR));
            label.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    stackframeClicked(i, debugInfo);
                }
            });
            return label;
        });


        splitter2.setSecondComponent(ScrollPaneFactory.createScrollPane(table));
    }

    private String getText(String text) {
        return StringUtils.replace(text, "\n", " ");
    }

    private void actionAccepted(SltDebugAction action, SltDebugInfo debugInfo) {
        int ix = debugInfo.getActions().indexOf(action);
        if (action.getArguments().isEmpty()) {
            try {
                SltSBCL.getInstance().sendToSbcl(SltInvokeNthRestart.nthRestart(debugInfo.getThreadId(),
                        BigInteger.valueOf(ix), debugInfo.getDebugLevel(), "NIL", "NIL", () -> {}));
            } catch (Exception e) {
                // TODO: log
            }
        } else {
            List<String> arguments = new ArrayList<>();
            String rest = "NIL";
            for (SltDebugArgument argument : action.getArguments()) {
                if (!argument.isRest()) {
                    String arg = Messages.showInputDialog("Single value or sexpression for argument: " + argument.getName(),
                            "Specify Restart Argument",null);
                    arguments.add(StringUtils.isBlank(arg) ? "NIL" : arg);
                } else {
                    String arg = Messages.showInputDialog("Expressions separated by space for rest argument " + argument.getName(),
                            "Specify Restart Rest Arguments",null);
                    if (StringUtils.isNotBlank(arg)) {
                        rest = "(" + arg + ")";
                    }
                }
            }
            String args = arguments.size() == 0 ? "NIL" : "(" + String.join(" ", arguments) + ")";
            try {
                SltSBCL.getInstance().sendToSbcl(SltInvokeNthRestart.nthRestart(debugInfo.getThreadId(),
                        BigInteger.valueOf(ix), debugInfo.getDebugLevel(), args, rest, () -> {}));
            } catch (Exception e) {
                // TODO: log
            }
        }

    }

    private void closeGui() {
        parent.removeDebugger(this, lastDebugId);
    }

    private void stackframeClicked(int i, SltDebugInfo debugInfo) {

    }

    private void close() {
        try {
            SltSBCL.getInstance().sendToSbcl(new ThrowToToplevel(lastDebugId));
        } catch (Exception e) {
            // TODO: log
        }
    }

    @Override
    public void activate() {
        this.tabInfo.fireAlert();
    }

    private static class SltDebuggerStackTraceTableModel extends AbstractTableModel {

        private final SltDebugInfo debugInfo;

        public SltDebuggerStackTraceTableModel(SltDebugInfo debugInfo) {
            this.debugInfo = debugInfo;
        }

        @Override
        public int getRowCount() {
            return debugInfo.getStacktrace().size();
        }

        @Override
        public int getColumnCount() {
            return 1;
        }

        @Override
        public Object getValueAt(int i, int i1) {
            return debugInfo.getStacktrace().get(i).getLine();
        }

        @Override
        public String getColumnName(int column) {
            if (column == 0) {
                return "Stacktrace";
            }
            return null;
        }

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            if (columnIndex == 0) {
                return String.class;
            }
            return null;
        }
    }
}

package com.en_circle.slt.plugin.ui.console;

import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.swank.SwankServer;
import com.en_circle.slt.plugin.swank.SwankServer.SwankServerOutput;
import com.en_circle.slt.plugin.swank.requests.SltEval;
import com.en_circle.slt.plugin.ui.SltComponent;
import com.intellij.execution.console.ConsoleExecuteAction;
import com.intellij.execution.console.ConsoleHistoryController;
import com.intellij.execution.console.LanguageConsoleBuilder;
import com.intellij.execution.console.LanguageConsoleBuilder.MyConsoleRootType;
import com.intellij.execution.console.LanguageConsoleView;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.tabs.TabInfo;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;

public class SltConsole implements SltComponent {
    private static final Logger LOG = LoggerFactory.getLogger(SltConsole.class);

    private TabInfo tabInfo;
    protected LanguageConsoleView languageConsole;
    private final JPanel content;
    protected final Project project;

    protected String currentModule = "cl-user";

    public SltConsole(Project project) {
        this.content = new JPanel(new BorderLayout());
        this.project = project;
    }

    @Override
    public TabInfo create() {
        languageConsole = new LanguageConsoleBuilder()
                .build(project, SltCommonLispLanguage.INSTANCE);
        languageConsole.setPrompt(currentModule + "> ");

        ConsoleExecuteAction action = new ConsoleExecuteAction(languageConsole, new SltConsoleExecuteActionHandler(languageConsole) {

            @Override
            protected void execute(String code) {
                eval(code);
            }

        }, languageConsoleView -> SwankServer.INSTANCE.isActive());
        action.registerCustomShortcutSet(action.getShortcutSet(), languageConsole.getConsoleEditor().getComponent());
        new ConsoleHistoryController(new MyConsoleRootType("cl"), null, languageConsole).install();

        content.add(languageConsole.getComponent(), BorderLayout.CENTER);

        tabInfo = new TabInfo(content);
        tabInfo.setText(getTitle());
        return tabInfo;
    }

    protected void eval(String data) {
        try {
            if (StringUtils.isNotBlank(data)) {
                SltSBCL.getInstance().sendToSbcl(SltEval.eval(data, currentModule,
                        result -> languageConsole.print(result + "\n", ConsoleViewContentType.NORMAL_OUTPUT)));
            }
        } catch (Exception e) {
            LOG.warn("Error starting sbcl", e);
            Messages.showErrorDialog(project, e.getMessage(), "Failed to Start SBCL");
        }
    }

    @Override
    public TabInfo getTabInfo() {
        return tabInfo;
    }

    @Override
    public void onPreStart() {

    }

    @Override
    public void onPostStart() {

    }

    @Override
    public void handleOutput(SwankServerOutput output, String data) {

    }

    @Override
    public void onPreStop() {

    }

    @Override
    public void onPostStop() {

    }

    @Override
    public String getTitle() {
        return "REPL";
    }

    public void close() {
        languageConsole.dispose();
    }
}

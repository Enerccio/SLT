package com.en_circle.slt.plugin.ui.console;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.plugin.swank.requests.Eval;
import com.en_circle.slt.plugin.ui.SltComponent;
import com.intellij.execution.console.ConsoleExecuteAction;
import com.intellij.execution.console.ConsoleHistoryController;
import com.intellij.execution.console.LanguageConsoleBuilder;
import com.intellij.execution.console.LanguageConsoleBuilder.MyConsoleRootType;
import com.intellij.execution.console.LanguageConsoleView;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.tabs.TabInfo;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;

public abstract class SltConsole implements SltComponent {
    private static final Logger log = LoggerFactory.getLogger(SltConsole.class);
    public static final String KEY = SltConsole.class.getName();

    private TabInfo tabInfo;
    protected LanguageConsoleView languageConsole;
    protected final JPanel content;
    protected final Project project;

    protected String currentPackage = "COMMON-LISP-USER";

    public SltConsole(Project project) {
        this.content = new JPanel(new BorderLayout());
        this.project = project;
    }

    protected void setPackage(String packageName) {
        if (packageName == null) {
            packageName = "COMMON-LISP-USER";
        }
        currentPackage = packageName;
        languageConsole.setPrompt(currentPackage + ">");
    }

    @Override
    public TabInfo create() {
        languageConsole = new LanguageConsoleBuilder()
                .build(project, SltCommonLispLanguage.INSTANCE);
        Disposer.register(project, languageConsole);
        languageConsole.setPrompt(currentPackage + "> ");

        ConsoleExecuteAction action = new ConsoleExecuteAction(languageConsole,
                new SltConsoleExecuteActionHandler(languageConsole, this) {

            @Override
            protected void execute(String code) {
                eval(code);
            }

        }, languageConsoleView -> LispEnvironmentService.getInstance(project).getState() == LispEnvironmentState.READY);
        action.registerCustomShortcutSet(action.getShortcutSet(), languageConsole.getConsoleEditor().getComponent());
        new ConsoleHistoryController(new MyConsoleRootType("cl"), null, languageConsole).install();

        content.add(languageConsole.getComponent(), BorderLayout.CENTER);
        content.putClientProperty(SltConsole.KEY, this);

        tabInfo = new TabInfo(content);
        tabInfo.setText(getTitle());
        return tabInfo;
    }

    protected void eval(String data) {
        try {
            if (StringUtils.isNotBlank(data)) {
                LispEnvironmentService.getInstance(project).sendToLisp(Eval.eval(data, currentPackage,
                        result -> languageConsole.print(result + "\n", ConsoleViewContentType.NORMAL_OUTPUT)));
            }
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.start"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
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
    public void handleOutput(SltOutput output, String data) {

    }

    @Override
    public void onPreStop() {

    }

    @Override
    public void onPostStop() {

    }

    @Override
    public String getTitle() {
        return SltBundle.message("slt.ui.repl.title");
    }

    public void close() {
        languageConsole.dispose();
    }

    public String getPackage() {
        return currentPackage;
    }
}

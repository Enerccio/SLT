package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.tabs.TabInfo;

import javax.swing.*;
import java.awt.*;

public class SltOutputHandlerComponent implements SltComponent, Disposable {

    private final SltOutput output;
    private final JPanel dataContainer;
    private TabInfo tabInfo;
    private ConsoleView consoleView;
    private final Project project;
    private boolean firstStart = true;

    public SltOutputHandlerComponent(Project project, SltOutput output) {
        this.output = output;
        this.project = project;
        this.dataContainer = new JPanel(new BorderLayout());
    }

    public SltOutput getOutput() {
        return output;
    }

    @Override
    public TabInfo create() {
        consoleView = TextConsoleBuilderFactory.getInstance()
                .createBuilder(project)
                .getConsole();
        Disposer.register(this, consoleView);
        dataContainer.add(consoleView.getComponent());

        tabInfo = new TabInfo(dataContainer);
        tabInfo.setText(getTitle());
        return tabInfo;
    }

    private ConsoleViewContentType getOutputType() {
        if (output == SltOutput.STDERR) {
            return ConsoleViewContentType.ERROR_OUTPUT;
        } else {
            return ConsoleViewContentType.SYSTEM_OUTPUT;
        }
    }

    @Override
    public TabInfo getTabInfo() {
        return tabInfo;
    }

    @Override
    public void onPreStart() {
        if (!firstStart) {
            consoleView.print("\n\n***\n\n", getOutputType());
        }
    }

    @Override
    public void onPostStart() {
        firstStart = false;
    }

    @Override
    public void handleOutput(SltOutput output, String data) {
        if (output == this.output) {
            consoleView.print(data, getOutputType());
        }
    }

    @Override
    public void onPreStop() {

    }

    @Override
    public void onPostStop() {

    }

    @Override
    public String getTitle() {
        return getOutput() == SltOutput.STDERR ? SltBundle.message("slt.ui.process.log.error.title") :
                SltBundle.message("slt.ui.process.log.output.title");
    }

    @Override
    public void dispose() {

    }
}

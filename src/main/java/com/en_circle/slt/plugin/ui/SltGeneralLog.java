package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.environment.SltLispEnvironment.SltOutput;
import com.en_circle.slt.plugin.swank.SlimeListener.RequestResponseLogger;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.tabs.TabInfo;
import org.apache.commons.lang3.StringUtils;

import javax.swing.*;
import java.awt.*;

public class SltGeneralLog implements SltComponent, RequestResponseLogger, Disposable {

    private final JPanel dataContainer;
    private ConsoleView consoleView;
    private TabInfo tabInfo;

    private final Project project;

    public SltGeneralLog(Project project) {
        this.project = project;
        this.dataContainer = new JPanel(new BorderLayout());
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
        return SltBundle.message("slt.ui.process.gl.title");
    }

    @Override
    public void logRequest(String request) {
        request = StringUtils.truncate(request, 0, 4069);
        consoleView.print("\n\n" + request, ConsoleViewContentType.LOG_INFO_OUTPUT);
    }

    @Override
    public void logResponse(String response) {
        response = StringUtils.truncate(response, 0, 4069);
        consoleView.print("\n\n" + response, ConsoleViewContentType.LOG_INFO_OUTPUT);
    }

    @Override
    public void dispose() {

    }
}

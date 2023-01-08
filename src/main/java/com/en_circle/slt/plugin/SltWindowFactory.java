package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.view.SltCoreWindow;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;

public class SltWindowFactory implements ToolWindowFactory {

    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        SltCoreWindow sltCoreWindow = new SltCoreWindow(toolWindow);
        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        Content content = contentFactory.createContent(sltCoreWindow.getContent(), "SBCL Process", false);
        toolWindow.getContentManager().addContent(content);
    }

}

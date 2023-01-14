package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.ui.SltCoreWindow;
import com.en_circle.slt.plugin.ui.debug.SltDebuggers;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;

public class SltWindowFactory implements ToolWindowFactory {

    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        {
            SltCoreWindow sltCoreWindow = new SltCoreWindow(toolWindow);
            ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
            Content content = contentFactory.createContent(sltCoreWindow.getContent(),
                    SltBundle.message("slt.ui.process.title"), false);
            toolWindow.getContentManager().addContent(content);
        }

        {
            SltDebuggers debugger = new SltDebuggers(toolWindow);
            ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
            Content content = contentFactory.createContent(debugger.getContent(),
                    SltBundle.message("slt.ui.debuggers.title"), false);
            debugger.setSelf(content);
            toolWindow.getContentManager().addContent(content);
        }
    }

}

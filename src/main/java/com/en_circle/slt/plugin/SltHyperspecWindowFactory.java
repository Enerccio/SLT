package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.ui.SltHyperspecView;
import com.en_circle.slt.plugin.ui.SltUIService;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;

public class SltHyperspecWindowFactory implements ToolWindowFactory, DumbAware {

    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        {
            SltHyperspecView hyperspecView = new SltHyperspecView(toolWindow);
            ContentFactory contentFactory = ContentFactory.getInstance();
            Content content = contentFactory.createContent(hyperspecView.getContent(),
                    SltBundle.message("slt.ui.clhs.title"), false);
            toolWindow.getContentManager().addContent(content);

            Disposer.register(SltUIService.getInstance(project), hyperspecView);
        }
    }

}

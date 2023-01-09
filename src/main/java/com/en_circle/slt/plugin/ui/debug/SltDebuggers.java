package com.en_circle.slt.plugin.ui.debug;

import com.intellij.openapi.wm.ToolWindow;

import javax.swing.*;
import java.awt.*;

public class SltDebuggers {

    private final ToolWindow toolWindow;
    private final JPanel content;

    public SltDebuggers(ToolWindow toolWindow) {
        this.toolWindow = toolWindow;
        this.content = new JPanel(new BorderLayout());
    }

    public JPanel getContent() {
        return content;
    }

}

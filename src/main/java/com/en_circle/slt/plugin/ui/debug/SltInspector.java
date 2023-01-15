package com.en_circle.slt.plugin.ui.debug;

import javax.swing.*;
import java.awt.*;

public class SltInspector {

    private final JPanel content;

    public SltInspector() {

        content = new JPanel(new BorderLayout());
    }

    public JComponent getContent() {
        return content;
    }

}

package com.en_circle.slt.plugin;

import com.intellij.ui.JBColor;

import java.awt.*;

public class SltUIConstants {

    public static final JBColor HYPERLINK_COLOR = new JBColor(new Color(0, 51, 204), new Color(128, 159, 255));
    public static final JBColor DEBUG_FRAMES_COLOR = new JBColor(new Color(230, 245, 95), new Color(5, 51, 12));
    public static final JBColor DEBUG_FRAMES_SELECTED_COLOR = new JBColor(new Color(230, 145, 95), new Color(5, 51, 72));
    public static final JBColor ERROR_BORDER_COLOR = new JBColor(new Color(176, 0, 32), new Color(207, 102, 121));

    public static String colorToHex(JBColor color) {
        int rgba = (color.getRGB() << 8) | color.getAlpha();
        return String.format("#%08X", rgba);
    }
}

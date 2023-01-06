package com.en_circle.slt.plugin;

import com.intellij.openapi.util.IconLoader;

import javax.swing.*;

public class SltIconProvider {

    public static final Icon file = IconLoader.getIcon("/icons/fileicon.svg", SltIconProvider.class);

    public static Icon getFileIcon() {
        return file;
    }

}

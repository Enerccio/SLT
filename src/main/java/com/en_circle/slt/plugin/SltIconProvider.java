package com.en_circle.slt.plugin;

import com.intellij.icons.AllIcons.Nodes;
import com.intellij.openapi.util.IconLoader;

import javax.swing.*;

public class SltIconProvider {

    public static final Icon file = IconLoader.getIcon("/icons/fileicon.svg", SltIconProvider.class);
    public static final Icon sbcl = IconLoader.getIcon("/icons/sbcl.png", SltIconProvider.class);

    public static final Icon MACRO = Nodes.Template;
    public static final Icon FUNCTION = Nodes.Function;
    public static final Icon METHOD = Nodes.Method;
    public static final Icon LAMBDA = Nodes.Lambda;
    public static final Icon CONSTANT = Nodes.Constant;
    public static final Icon SPECIAL = Nodes.Gvariable;
    public static final Icon CLASS = Nodes.Class;
    public static final Icon PACKAGE = Nodes.Package;
    public static final Icon STRUCTURE = Nodes.Static;
    public static final Icon TYPE = Nodes.Type;

    public static Icon getFileIcon() {
        return file;
    }

    public static Icon getSbclIcon() {
        return sbcl;
    }
}

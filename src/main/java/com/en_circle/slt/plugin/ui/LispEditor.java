package com.en_circle.slt.plugin.ui;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.intellij.ui.EditorTextField;

public class LispEditor extends EditorTextField {

    public LispEditor() {
        setFileType(SltCommonLispFileType.INSTANCE);
    }

}

package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.swank.SwankServer;
import com.intellij.lang.Language;

public class SltCommonLispLanguage extends Language {

    public static final String ID = "Common Lisp";
    public static final SltCommonLispLanguage INSTANCE = new SltCommonLispLanguage();

    public SltCommonLispLanguage() {
        super(ID);
    }

    @Override
    public boolean isCaseSensitive() {
        return false;
    }

}

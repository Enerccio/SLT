package com.en_circle.slt.plugin;

import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.util.NlsContexts.Label;
import com.intellij.openapi.util.NlsSafe;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class SltCommonLispFileType extends LanguageFileType {

    public static final SltCommonLispFileType INSTANCE = new SltCommonLispFileType();

    public SltCommonLispFileType() {
        super(SltCommonLispLanguage.INSTANCE);
    }

    @Override
    public @NonNls @NotNull String getName() {
        return SltCommonLispLanguage.ID;
    }

    @Override
    public @Label @NotNull String getDescription() {
        return SltBundle.message("slt.file.description");
    }

    @Override
    public @NlsSafe @NotNull String getDefaultExtension() {
        return "cl";
    }

    @Override
    public Icon getIcon() {
        return SltIconProvider.getFileIcon();
    }
}

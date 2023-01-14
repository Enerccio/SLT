package com.en_circle.slt.plugin.highlights;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltIconProvider;
import com.en_circle.slt.templates.CodeHighlightTemplate;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import com.intellij.openapi.util.NlsContexts.ConfigurableName;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

public class SltColorSettingsPage implements ColorSettingsPage {

    public static final String DEMO_CODE = new CodeHighlightTemplate().render();

    private static final AttributesDescriptor[] DESCRIPTORS = new AttributesDescriptor[] {
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.parenthesis"), CommonLispHighlighterColors.PARENTS),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.number"), CommonLispHighlighterColors.NUMBER),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.stringliteral"), CommonLispHighlighterColors.STRING),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.comment"), CommonLispHighlighterColors.COMMENT),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.keyword"), CommonLispHighlighterColors.KEYWORD),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.helper"), CommonLispHighlighterColors.DEFUN_FORM),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.specvariable"), CommonLispHighlighterColors.SPECIAL_VARIABLE),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.constant"), CommonLispHighlighterColors.CONSTANT),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.specialform"), CommonLispHighlighterColors.SPECIAL_FORM),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.function"), CommonLispHighlighterColors.FUNCTION),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.macro"), CommonLispHighlighterColors.MACRO),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.reader"), CommonLispHighlighterColors.SUGAR),
    };

    @Override
    public @Nullable Icon getIcon() {
        return SltIconProvider.getFileIcon();
    }

    @Override
    public @NotNull SyntaxHighlighter getHighlighter() {
        return new CommonLispStaticHighlighter();
    }

    @Override
    public @NonNls @NotNull String getDemoText() {
        return DEMO_CODE;
    }

    @Override
    public @Nullable Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        return null;
    }

    @Override
    public AttributesDescriptor @NotNull [] getAttributeDescriptors() {
        return DESCRIPTORS;
    }

    @Override
    public ColorDescriptor @NotNull [] getColorDescriptors() {
        return ColorDescriptor.EMPTY_ARRAY;
    }

    @Override
    public @NotNull @ConfigurableName String getDisplayName() {
        return SltBundle.message("slt.commonlisp");
    }
}

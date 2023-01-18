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
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.parenthesis"), SltHighlighterColors.PARENTS),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.number"), SltHighlighterColors.NUMBER),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.stringliteral"), SltHighlighterColors.STRING),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.comment"), SltHighlighterColors.COMMENT),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.keyword"), SltHighlighterColors.KEYWORD),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.helper"), SltHighlighterColors.DEFUN_FORM),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.specvariable"), SltHighlighterColors.SPECIAL_VARIABLE),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.constant"), SltHighlighterColors.CONSTANT),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.specialform"), SltHighlighterColors.SPECIAL_FORM),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.function"), SltHighlighterColors.FUNCTION),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.macro"), SltHighlighterColors.MACRO),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.reader"), SltHighlighterColors.SUGAR),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.class"), SltHighlighterColors.CLASS),
            new AttributesDescriptor(SltBundle.message("slt.ui.colorsettings.method"), SltHighlighterColors.METHOD),
    };

    @Override
    public @Nullable Icon getIcon() {
        return SltIconProvider.getFileIcon();
    }

    @Override
    public @NotNull SyntaxHighlighter getHighlighter() {
        return new SltStaticHighlighter();
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

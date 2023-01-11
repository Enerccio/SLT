package com.en_circle.slt.plugin.highlights;

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
            new AttributesDescriptor("Parenthesis", CommonLispHighlighterColors.PARENTS),
            new AttributesDescriptor("Number", CommonLispHighlighterColors.NUMBER),
            new AttributesDescriptor("String literal", CommonLispHighlighterColors.STRING),
            new AttributesDescriptor("Comment", CommonLispHighlighterColors.COMMENT),
            new AttributesDescriptor("Keyword symbol", CommonLispHighlighterColors.KEYWORD),
            new AttributesDescriptor("Helper symbol", CommonLispHighlighterColors.DEFUN_FORM),
            new AttributesDescriptor("Special variable", CommonLispHighlighterColors.SPECIAL_VARIABLE),
            new AttributesDescriptor("Constant variable", CommonLispHighlighterColors.CONSTANT),
            new AttributesDescriptor("Special form", CommonLispHighlighterColors.SPECIAL_FORM),
            new AttributesDescriptor("Function call", CommonLispHighlighterColors.FUNCTION),
            new AttributesDescriptor("Macro call", CommonLispHighlighterColors.MACRO),
            new AttributesDescriptor("Reader macro", CommonLispHighlighterColors.SUGAR),
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
        return "Common Lisp";
    }
}

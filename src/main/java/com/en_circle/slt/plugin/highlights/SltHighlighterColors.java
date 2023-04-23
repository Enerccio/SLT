package com.en_circle.slt.plugin.highlights;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.psi.PsiElement;

public class SltHighlighterColors {

    public static TextAttributesKey COMMENT = TextAttributesKey.createTextAttributesKey("CL.COMMENT", DefaultLanguageHighlighterColors.BLOCK_COMMENT);
    public static TextAttributesKey PARENTS = TextAttributesKey.createTextAttributesKey("CL.PARENTS", DefaultLanguageHighlighterColors.PARENTHESES);
    public static TextAttributesKey NUMBER = TextAttributesKey.createTextAttributesKey("CL.NUMBER", DefaultLanguageHighlighterColors.NUMBER);
    public static TextAttributesKey STRING = TextAttributesKey.createTextAttributesKey("CL.STRING", DefaultLanguageHighlighterColors.STRING);
    public static TextAttributesKey KEYWORD = TextAttributesKey.createTextAttributesKey("CL.KEYWORD", DefaultLanguageHighlighterColors.METADATA);
    public static TextAttributesKey DEFUN_FORM = TextAttributesKey.createTextAttributesKey("CL.DEFUN_FORM", DefaultLanguageHighlighterColors.METADATA);
    public static TextAttributesKey SPECIAL_VARIABLE = TextAttributesKey.createTextAttributesKey("CL.DYNAMIC", DefaultLanguageHighlighterColors.GLOBAL_VARIABLE);
    public static TextAttributesKey SUGAR = TextAttributesKey.createTextAttributesKey("CL.SUGAR", DefaultLanguageHighlighterColors.PREDEFINED_SYMBOL);
    public static TextAttributesKey CONSTANT = TextAttributesKey.createTextAttributesKey("CL.CONSTANT", DefaultLanguageHighlighterColors.CONSTANT);
    public static TextAttributesKey SPECIAL_FORM = TextAttributesKey.createTextAttributesKey("CL.SPECIAL_FORM", DefaultLanguageHighlighterColors.KEYWORD);
    public static TextAttributesKey FUNCTION = TextAttributesKey.createTextAttributesKey("CL.FUNCTION", DefaultLanguageHighlighterColors.FUNCTION_CALL);
    public static TextAttributesKey MACRO = TextAttributesKey.createTextAttributesKey("CL.MACRO", DefaultLanguageHighlighterColors.KEYWORD);
    public static TextAttributesKey CLASS = TextAttributesKey.createTextAttributesKey("CL.CLASS", DefaultLanguageHighlighterColors.CLASS_NAME);
    public static TextAttributesKey METHOD = TextAttributesKey.createTextAttributesKey("CL.METHOD", DefaultLanguageHighlighterColors.INSTANCE_METHOD);
    public static TextAttributesKey QUOTED = TextAttributesKey.createTextAttributesKey("CL.QUOTED");

    public static void setHighlighting(PsiElement element, AnnotationHolder holder, TextAttributesKey key) {
        holder.newSilentAnnotation(HighlightSeverity.INFORMATION)
                .range(element)
                .textAttributes(key)
                .needsUpdateOnTyping(false)
                .create();
    }

    public static void setHighlightingStrict(PsiElement element, AnnotationHolder holder, TextAttributesKey key) {
        holder.newSilentAnnotation(HighlightSeverity.WEAK_WARNING)
                .range(element)
                .enforcedTextAttributes(EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key))
                .needsUpdateOnTyping(false)
                .create();
    }

    public static void setHighlightingTextAttribute(PsiElement element, AnnotationHolder holder, TextAttributes textAttributes) {
        holder.newSilentAnnotation(HighlightSeverity.WEAK_WARNING)
                .range(element)
                .enforcedTextAttributes(textAttributes)
                .needsUpdateOnTyping(false)
                .create();
    }

}

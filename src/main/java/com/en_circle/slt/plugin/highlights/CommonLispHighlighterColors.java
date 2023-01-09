package com.en_circle.slt.plugin.highlights;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;

public class CommonLispHighlighterColors {

    public static TextAttributesKey COMMENT = TextAttributesKey.createTextAttributesKey("CL.COMMENT", DefaultLanguageHighlighterColors.BLOCK_COMMENT);
    public static TextAttributesKey PARENTS = TextAttributesKey.createTextAttributesKey("CL.PARENTS", DefaultLanguageHighlighterColors.PARENTHESES);
    public static TextAttributesKey NUMBER = TextAttributesKey.createTextAttributesKey("CL.NUMBER", DefaultLanguageHighlighterColors.NUMBER);

    public static TextAttributesKey SPECIAL_FORM = TextAttributesKey.createTextAttributesKey("CL.SPECIAL_FORM", DefaultLanguageHighlighterColors.KEYWORD);
    public static TextAttributesKey FUNCTION = TextAttributesKey.createTextAttributesKey("CL.FUNCTION", DefaultLanguageHighlighterColors.FUNCTION_CALL);
    public static TextAttributesKey MACRO = TextAttributesKey.createTextAttributesKey("CL.MACRO", DefaultLanguageHighlighterColors.KEYWORD);

    public static void setHighlighting(PsiElement element, AnnotationHolder holder, TextAttributesKey key) {
//        Annotation a = holder.createInfoAnnotation(element, null);
//        a.setTextAttributes(key);
//        a.setNeedsUpdateOnTyping(true);

        holder.newSilentAnnotation(HighlightSeverity.INFORMATION)
                .range(element)
                .textAttributes(key)
                .needsUpdateOnTyping(false)
                .create();
    }

    public static void setHighlightingStrict(PsiElement element, AnnotationHolder holder, TextAttributesKey key) {
//        val annotation = holder.createInfoAnnotation(element, null)
//        annotation.enforcedTextAttributes = TextAttributes.ERASE_MARKER
//        annotation.enforcedTextAttributes = EditorColorsManager.getInstance().globalScheme.getAttributes(key)
//        annotation.setNeedsUpdateOnTyping(false)

        holder.newSilentAnnotation(HighlightSeverity.WEAK_WARNING)
                .range(element)
                .enforcedTextAttributes(EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key))
                .needsUpdateOnTyping(false)
                .create();
    }

}

package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.LispParserUtil.LispSexpressionInfo;
import com.en_circle.slt.plugin.lisp.LispParserUtil.SexpressionType;
import com.en_circle.slt.plugin.lisp.psi.LispFile;
import com.en_circle.slt.plugin.lisp.psi.LispSexpr;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.intellij.ide.navigationToolbar.StructureAwareNavBarModelExtension;
import com.intellij.lang.Language;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class SltStructureAwareNavbar extends StructureAwareNavBarModelExtension {

    @NotNull
    @Override
    protected Language getLanguage() {
        return SltCommonLispLanguage.INSTANCE;
    }

    @Override
    public @Nullable Icon getIcon(Object object) {
        if (object instanceof LispFile) {
            return SltIconProvider.getFileIcon();
        }
        LispSexpressionInfo topLevelInfo = getTopLevelForObject(object);
        if (topLevelInfo != null) {
            return topLevelInfo.getIcon();
        }
        if (object instanceof PsiElement psiElement) {
            if (psiElement.getContainingFile() instanceof LispFile) {
                return SltIconProvider.getFileIcon();
            }
        }
        return null;
    }

    @Override
    public @Nullable String getPresentableText(Object object) {
        if (object instanceof LispFile file) {
            return file.getName();
        }
        LispSexpressionInfo topLevelInfo = getTopLevelForObject(object);
        if (topLevelInfo != null) {
            return topLevelInfo.getLongForm();
        }
        if (object instanceof PsiElement psiElement) {
            if (psiElement.getContainingFile() instanceof LispFile file) {
                return file.getName();
            }
        }
        return null;
    }

    private LispSexpressionInfo getTopLevelForObject(Object object) {
        if (object instanceof LispToplevel toplevel) {
            return LispParserUtil.determineTopLevelType(toplevel.getSexpr());
        }
        if (object instanceof LispSexpr sexpr) {
            LispSexpressionInfo info = LispParserUtil.determineTopLevelType(sexpr);
            if (info != null && info.getType() != SexpressionType.EXPRESSION) {
                return info;
            }
        }
        if (object instanceof PsiElement psiElement) {
            LispSexpr sexpr = PsiTreeUtil.getParentOfType(psiElement, LispSexpr.class);
            return getTopLevelForObject(sexpr);
        }
        return null;
    }

}

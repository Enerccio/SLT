package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.LispParserUtil.LispSexpressionInfo;
import com.en_circle.slt.plugin.lisp.LispParserUtil.SexpressionType;
import com.en_circle.slt.plugin.lisp.psi.LispFile;
import com.en_circle.slt.plugin.lisp.psi.LispSexpr;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.intellij.ide.navigationToolbar.StructureAwareNavBarModelExtension;
import com.intellij.lang.Language;
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
        if (object instanceof LispToplevel toplevel) {
            LispSexpressionInfo info =  LispParserUtil.determineTopLevelType(toplevel.getSexpr());
            if (info.getType() != SexpressionType.EXPRESSION) {
                return info.getIcon();
            }
            return SltIconProvider.getFileIcon();
        }
        if (object instanceof LispSexpr sexpr) {
            LispSexpressionInfo info =  LispParserUtil.determineTopLevelType(sexpr);
            if (info.getType() != SexpressionType.EXPRESSION) {
                return info.getIcon();
            }
            return SltIconProvider.getFileIcon();
        }
        return null;
    }

    @Override
    public @Nullable String getPresentableText(Object object) {
        if (object instanceof LispFile file) {
            return file.getName();
        }
        if (object instanceof LispToplevel toplevel) {
            LispSexpressionInfo info =  LispParserUtil.determineTopLevelType(toplevel.getSexpr());
            if (info.getType() != SexpressionType.EXPRESSION) {
                return info.getLongForm();
            }
            return toplevel.getContainingFile().getName();
        }
        if (object instanceof LispSexpr sexpr) {
            LispSexpressionInfo info =  LispParserUtil.determineTopLevelType(sexpr);
            if (info.getType() != SexpressionType.EXPRESSION) {
                return info.getLongForm();
            }
            return sexpr.getContainingFile().getName();
        }
        return null;
    }
}

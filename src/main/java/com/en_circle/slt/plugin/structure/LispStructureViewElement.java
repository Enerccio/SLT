package com.en_circle.slt.plugin.structure;

import com.en_circle.slt.plugin.SltIconProvider;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.LispParserUtil.LispSexpressionInfo;
import com.en_circle.slt.plugin.lisp.LispParserUtil.SexpressionType;
import com.en_circle.slt.plugin.lisp.psi.LispFile;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.util.treeView.smartTree.SortableTreeElement;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

public class LispStructureViewElement implements StructureViewTreeElement, SortableTreeElement {

    private final NavigatablePsiElement psiElement;
    private final LispSexpressionInfo sexpressionInfo;
    private final ItemPresentation presentation;

    public LispStructureViewElement(NavigatablePsiElement psiElement, LispSexpressionInfo sexpressionInfo) {
        this.psiElement = psiElement;
        this.sexpressionInfo = sexpressionInfo;
        presentation = new ItemPresentation() {
            @Override
            public @NlsSafe @Nullable String getPresentableText() {
                if (psiElement instanceof LispFile file) {
                    return file.getName();
                } else if (sexpressionInfo != null) {
                    return sexpressionInfo.getLongForm();
                }
                return null;
            }

            @Override
            public @Nullable Icon getIcon(boolean unused) {
                if (psiElement instanceof LispFile) {
                    return SltIconProvider.getFileIcon();
                } else if (sexpressionInfo != null) {
                    return sexpressionInfo.getIcon();
                }
                return null;
            }
        };
    }

    @Override
    public Object getValue() {
        return psiElement;
    }

    @Override
    public @NotNull String getAlphaSortKey() {
        if (sexpressionInfo != null) {
            return sexpressionInfo.getIdentification();
        }
        return "";
    }

    @Override
    public @NotNull ItemPresentation getPresentation() {
        return presentation;
    }

    @Override
    public TreeElement @NotNull [] getChildren() {
        if (psiElement instanceof LispFile) {
            List<LispStructureViewElement> elementList = new ArrayList<>();
            LispToplevel[] toplevels = PsiTreeUtil.getChildrenOfType(psiElement, LispToplevel.class);
            if (toplevels != null) {
                for (LispToplevel toplevel : toplevels) {
                    LispSexpressionInfo sexpressionInfo = LispParserUtil.determineTopLevelType(toplevel.getSexpr());
                    if (sexpressionInfo.getType() != SexpressionType.EXPRESSION) {
                        elementList.add(new LispStructureViewElement(toplevel, sexpressionInfo));
                    }
                }
            }

            return elementList.toArray(new TreeElement[0]);
        }
        return EMPTY_ARRAY;
    }

    @Override
    public void navigate(boolean requestFocus) {
        psiElement.navigate(requestFocus);
    }

    @Override
    public boolean canNavigate() {
        return psiElement.canNavigate();
    }

    @Override
    public boolean canNavigateToSource() {
        return psiElement.canNavigateToSource();
    }
}

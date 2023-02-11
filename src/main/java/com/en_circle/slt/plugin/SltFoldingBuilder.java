package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.intellij.lang.ASTNode;
import com.intellij.lang.folding.FoldingBuilderEx;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.FoldingGroup;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class SltFoldingBuilder extends FoldingBuilderEx implements DumbAware {

    @Override
    public FoldingDescriptor @NotNull [] buildFoldRegions(@NotNull PsiElement root, @NotNull Document document, boolean quick) {
        List<FoldingDescriptor> descriptors = new ArrayList<>();

        Collection<LispToplevel> toplevels =
                PsiTreeUtil.findChildrenOfType(root, LispToplevel.class);

        for (LispToplevel toplevel : toplevels) {
            FoldingGroup group = FoldingGroup.newGroup("SLTFoldTopLevel " + toplevel.hashCode());
            if (toplevel.getSexpr().getDatum() != null && toplevel.getSexpr().getDatum().getList() != null) {
                if (toplevel.getTextRange().getLength() > 2) {
                    FoldingDescriptor descriptor = new FoldingDescriptor(toplevel.getNode(),
                            new TextRange(toplevel.getTextRange().getStartOffset() + 1,
                                    toplevel.getTextRange().getEndOffset() - 1), group);
                    descriptors.add(descriptor);
                }
            }
        }

        return descriptors.toArray(new FoldingDescriptor[0]);
    }

    @Override
    public @Nullable String getPlaceholderText(@NotNull ASTNode node) {
        if (node.getPsi() instanceof LispToplevel toplevel) {
            return LispParserUtil.determineTopLevelType(toplevel.getSexpr()).getShortForm();
        }
        return "...";
    }

    @Override
    public boolean isCollapsedByDefault(@NotNull ASTNode node) {
        return false;
    }
}

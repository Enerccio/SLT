package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.lisp.psi.LispSexpr;
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
        String text = "...";
        if (node.getPsi() instanceof LispToplevel toplevel) {
            if (toplevel.getSexpr().getDatum() != null && toplevel.getSexpr().getDatum().getList() != null) {
                LispList list = toplevel.getSexpr().getDatum().getList();
                List<LispSexpr> elements = list.getSexprList();
                if (elements.size() > 0) {
                    LispSexpr head = elements.get(0);
                    if (head.getDatum() != null && head.getDatum().getCompoundSymbol() != null) {
                        String headName = head.getText();
                        text = headName;

                        if (headName.toLowerCase().startsWith("def") && elements.size() > 1) {
                            text += " " + elements.get(1).getText();

                            if (headName.equalsIgnoreCase("defmethod") && elements.size() > 2) {
                                LispSexpr potentialArgs = elements.get(2);
                                if (potentialArgs.getDatum() != null && potentialArgs.getDatum().getCompoundSymbol() != null) {
                                    text += " " + potentialArgs.getText();
                                }
                            }
                        }
                    }
                }

                return text;
            }
        }
        return "...";
    }

    @Override
    public boolean isCollapsedByDefault(@NotNull ASTNode node) {
        return false;
    }
}

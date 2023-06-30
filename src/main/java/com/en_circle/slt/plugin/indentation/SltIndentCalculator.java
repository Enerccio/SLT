package com.en_circle.slt.plugin.indentation;

import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;

import java.util.Objects;

public class SltIndentCalculator {

    private final Project project;

    public SltIndentCalculator(Project project) {
        this.project = project;
    }

    public Integer calculateIndent(PsiFile file, int offset, String wholeText,
                                   String packageOverride) {
        int initialOffset = offset;

        boolean wasAfter = false;
        PsiElement element = file.findElementAt(offset);
        while (element == null) {
            if (offset == 0) {
                return 0;
            }
            element = file.findElementAt(offset--);
            wasAfter = true;
        }

        while (element instanceof PsiWhiteSpace ||
                (element != null && element.getNode().getElementType() == LispTypes.LINE_COMMENT)) {
            if (offset == 0) {
                return 0;
            }
            element = file.findElementAt(offset--);
            wasAfter = true;
        }

        while ((element != null && element.getNode().getElementType() == LispTypes.LINE_COMMENT)) {
            if (offset == 0) {
                return 0;
            }
            element = file.findElementAt(offset--);
        }

        if (element == null) {
            return null;
        }

        if (element.getNode().getElementType() == LispTypes.LPAREN) {
            // left parenthesis means we need to check if we are toplevel last next element
            // in which case we are moving to new line
            if (element.getParent() == file) {
                // incomplete
                PsiElement previousToplevel = element.getPrevSibling();
                while (!(previousToplevel instanceof LispToplevel)) {
                    if (previousToplevel == null)
                        break;
                    previousToplevel = previousToplevel.getPrevSibling();
                }
                if (previousToplevel != null) {
                    if (previousToplevel.getNextSibling() == element ||
                            PsiTreeUtil.firstChild(previousToplevel.getNextSibling()) == element) {
                        // we are first ( of next toplevel
                        return 0;
                    }
                }
            } else {
                PsiElement topLevel = PsiTreeUtil.getParentOfType(element, LispToplevel.class);
                PsiManager manager = PsiManager.getInstance(element.getProject());
                if (topLevel != null) {
                    if (manager.areElementsEquivalent(topLevel.getFirstChild(), element)
                            || manager.areElementsEquivalent(PsiTreeUtil.firstChild(topLevel), element)) {
                        // we are first ( of next toplevel
                        return 0;
                    }
                }
            }
        }

        if (element.getNode().getElementType() == LispTypes.BLOCK_COMMENT) {
            return null;
        }

        return LispEnvironmentService.getInstance(Objects.requireNonNull(project))
                .calculateOffset(element, file, wasAfter, wholeText, initialOffset,
                        packageOverride);
    }

}

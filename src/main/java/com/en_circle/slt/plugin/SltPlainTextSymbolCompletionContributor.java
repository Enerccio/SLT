package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.LispParserUtil.LispSexpressionInfo;
import com.en_circle.slt.plugin.lisp.LispParserUtil.SexpressionType;
import com.en_circle.slt.plugin.lisp.psi.LispFile;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.intellij.codeInsight.completion.PlainTextSymbolCompletionContributor;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class SltPlainTextSymbolCompletionContributor implements PlainTextSymbolCompletionContributor {

    @Override
    public @NotNull Collection<LookupElement> getLookupElements(@NotNull PsiFile file,
                                                                int invocationCount, @NotNull String prefix) {
        if (file instanceof LispFile lispFile) {
            List<LookupElement> lookupElements = new ArrayList<>();
            for (LispToplevel toplevel : PsiTreeUtil.findChildrenOfType(lispFile, LispToplevel.class)) {
                LispSexpressionInfo sexpressionInfo = LispParserUtil.determineTopLevelType(toplevel.getSexpr());
                if (sexpressionInfo.getType() != SexpressionType.EXPRESSION) {
                    if (sexpressionInfo.getIdentification() != null && sexpressionInfo.getIdentification().startsWith(prefix)) {
                        LookupElement lookupElement = LookupElementBuilder.create(sexpressionInfo.getIdentification())
                                .withIcon(sexpressionInfo.getIcon())
                                .withLookupString(sexpressionInfo.getIdentification())
                                .withPsiElement(toplevel);
                        lookupElements.add(lookupElement);
                    }
                }
            }
            return lookupElements;
        }
        return Collections.emptySet();
    }

}

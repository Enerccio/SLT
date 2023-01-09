package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.lisp.psi.impl.LispPsiImplUtil;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.util.text.HtmlChunk;
import com.intellij.psi.PsiElement;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

public class SltDocumentationProvider extends AbstractDocumentationProvider {

    @Override
    public @Nullable @Nls String getQuickNavigateInfo(PsiElement element, PsiElement originalElement) {
        String text = LispPsiImplUtil.getSExpressionHead(element);
        if (text != null) {
            SymbolState state = SltSBCL.getInstance().refreshSymbolFromServer(SltSBCL.getInstance().getGlobalPackage(), text, element);
            switch (state.binding) {
                case NONE:
                    return "Symbol " + text;
                case FUNCTION:
                    return "Function " + text;
                case MACRO:
                    return "Macro " + text;
                case SPECIAL_FORM:
                    return "Special Form " + text;
            }
        }
        return null;
    }

    @Override
    public @Nullable @Nls String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
        String text = LispPsiImplUtil.getSExpressionHead(element);
        if (text != null) {
            SymbolState state = SltSBCL.getInstance().refreshSymbolFromServer(SltSBCL.getInstance().getGlobalPackage(), text, element);
            return asHtml(state.documentation);
        }
        return null;
    }

    private String asHtml(String documentation) {
        if (documentation == null)
            return null;

        return StringUtils.replace(documentation, "\n", HtmlChunk.br().toString());
    }
}

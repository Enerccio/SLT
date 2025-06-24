package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.environment.LispFeatures;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.tools.EscapeUtils;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.util.text.HtmlBuilder;
import com.intellij.openapi.util.text.HtmlChunk;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

public class SltDocumentationProvider extends AbstractDocumentationProvider {

    @Override
    public @Nullable @Nls String getQuickNavigateInfo(PsiElement element, PsiElement originalElement) {
        if (originalElement != null)
            element = decideOnElement(element, originalElement);

        if (!(element instanceof LispSymbol))
            element = PsiTreeUtil.getParentOfType(element, LispSymbol.class);

        if (element != null) {
            String text = ((LispSymbol) element).getName();
            String packageName = LispParserUtil.getPackage(element);
            SymbolState state = LispEnvironmentService.getInstance(element.getProject()).refreshSymbolFromServer(packageName, text);
            switch (state.binding) {
                case NONE:
                    return SltBundle.message("slt.documentation.types.symbol") + " " + text;
                case FUNCTION:
                    return SltBundle.message("slt.documentation.types.function") + " " + text;
                case MACRO:
                    return SltBundle.message("slt.documentation.types.macro") + " " + text;
                case SPECIAL_FORM:
                    return SltBundle.message("slt.documentation.types.specialform") + " " + text;
                case CONSTANT:
                    return SltBundle.message("slt.documentation.types.constant") + " " + text;
                case SPECIAL_VARIABLE:
                    return SltBundle.message("slt.documentation.types.specvariable") + " " + text;
                case KEYWORD:
                    return SltBundle.message("slt.documentation.types.keyword") + " " + text;
                case CLASS:
                    return SltBundle.message("slt.documentation.types.class") + " " + text;
                case METHOD:
                    return SltBundle.message("slt.documentation.types.method") + " " + text;
            }
        }
        return null;
    }

    @Override
    public @Nullable @Nls String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
        if (LispEnvironmentService.getInstance(element.getProject()).hasFeature(LispFeatures.DOCUMENTATION) ||
                LispEnvironmentService.getInstance(element.getProject()).hasFeature(LispFeatures.MACROEXPAND)) {
            if (originalElement != null)
                element = decideOnElement(element, originalElement);

            if (!(element instanceof LispSymbol))
                element = PsiTreeUtil.getParentOfType(element, LispSymbol.class);

            if (element != null) {
                String text = element.getText();
                String packageName = LispParserUtil.getPackage(element);
                SymbolState state = LispEnvironmentService.getInstance(element.getProject()).refreshSymbolFromServer(packageName, text);
                return asHtml(state, element);
            }
        }
        return null;
    }

    private PsiElement decideOnElement(PsiElement element, PsiElement originalElement) {
        if (element == null)
            return originalElement;
        if (element instanceof LispSymbol)
            return element;
        return originalElement;
    }

    private String asHtml(SymbolState state, PsiElement element) {
        HtmlBuilder builder = new HtmlBuilder();
        if (LispEnvironmentService.getInstance(element.getProject()).hasFeature(LispFeatures.DOCUMENTATION)) {
            String documentation = StringUtils.replace(StringUtils.replace(escape(state.documentation), " ", "&nbsp;"),
                    "\n", HtmlChunk.br().toString());
            builder.append(documentation == null ? HtmlChunk.raw("") :
                    HtmlChunk.raw(documentation));
        }

        String doc = builder.toString();
        return StringUtils.isBlank(doc) ? null : doc;
    }


    private String escape(String s) {
        return EscapeUtils.escapeHtml4(s);
    }
}

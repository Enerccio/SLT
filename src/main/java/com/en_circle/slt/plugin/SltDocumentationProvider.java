package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.SymbolState.SymbolBinding;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.impl.LispPsiImplUtil;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.util.text.HtmlBuilder;
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
            String packageName = LispParserUtil.getPackage(element);
            SymbolState state = SltLispEnvironmentProvider.getInstance().refreshSymbolFromServer(packageName, text, element);
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
            }
        }
        return null;
    }

    @Override
    public @Nullable @Nls String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
        if (element instanceof LispSymbol) {
            String text = element.getText();
            String packageName = LispParserUtil.getPackage(element);
            SymbolState state = SltLispEnvironmentProvider.getInstance().refreshSymbolFromServer(packageName, text, element);
            return asHtml(state, packageName, element, originalElement);
        }
        return null;
    }

    private String asHtml(SymbolState state, String packageName, PsiElement element, @Nullable PsiElement originalElement) {
        HtmlBuilder builder = new HtmlBuilder();
        String documentation = StringUtils.replace(StringUtils.replace(state.documentation, " ", "&nbsp;"),
                "\n", HtmlChunk.br().toString());
        builder.append(documentation == null ? HtmlChunk.raw("") : HtmlChunk.raw(documentation));

        LispList form = LispParserUtil.getIfHead(element);
        if (form != null && state.binding == SymbolBinding.MACRO) {
            String macroExpand = SltLispEnvironmentProvider.getInstance().macroexpand(form, packageName);
            if (macroExpand != null) {
                macroExpand = StringUtils.replace(StringUtils.replace(macroExpand, " ", "&nbsp;"),
                        "\n", HtmlChunk.br().toString());
                builder.append(HtmlChunk.hr());
                builder.append(HtmlChunk.text(SltBundle.message("slt.documentation.macroexpand")));
                builder.append(HtmlChunk.br());
                builder.append(HtmlChunk.raw(macroExpand));
            } else {
                builder.append(HtmlChunk.hr());
                builder.append(HtmlChunk.text(SltBundle.message("slt.documentation.macroexpand.generating")));
            }
        }

        String doc = builder.toString();
        return StringUtils.isBlank(doc) ? null : doc;
    }
}

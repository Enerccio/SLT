package com.en_circle.slt.plugin.params;

import com.en_circle.slt.plugin.environment.LispFeatures;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.LispParserUtil.QuoteState;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.lisp.psi.LispSexpr;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.plugin.swank.requests.Argslist;
import com.en_circle.slt.tools.SltApplicationUtils;
import com.intellij.lang.parameterInfo.CreateParameterInfoContext;
import com.intellij.lang.parameterInfo.ParameterInfoHandler;
import com.intellij.lang.parameterInfo.ParameterInfoUIContext;
import com.intellij.lang.parameterInfo.UpdateParameterInfoContext;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class SltParameterInfoHandler implements ParameterInfoHandler<LispList, LispArgslist> {

    @Override
    public @Nullable LispList findElementForParameterInfo(@NotNull CreateParameterInfoContext context) {
        LispList candidate = findElementAtOffset(context.getFile(), context.getOffset());
        if (candidate != null) {
            if (LispEnvironmentService.getInstance(context.getProject()).getState() == LispEnvironmentState.READY &&
                    LispEnvironmentService.getInstance(context.getProject()).hasFeature(LispFeatures.FUNC_ARGS)) {
                LispSymbol symbol = getHead(candidate);
                if (symbol == null)
                    return null;

                String packageName = LispParserUtil.getPackage(candidate);
                Object[] items = SltApplicationUtils.getAsyncResultNoThrow(context.getProject(), finishRequest -> Argslist
                        .getArgslist(symbol.getName(), packageName,
                                result -> finishRequest.accept(getCandidateObjects(result))));
                if (items != null) {
                    context.setItemsToShow(items);
                    return candidate;
                }
            }
        }
        return null;
    }

    private LispSymbol getHead(LispList candidate) {
        if (candidate.getSexprList().size() > 0) {
            LispSexpr sexpr = candidate.getSexprList().get(0);
            if (sexpr.getDatum() != null && sexpr.getDatum().getCompoundSymbol() != null) {
                return sexpr.getDatum().getCompoundSymbol().getSymbol();
            }
        }
        return null;
    }

    private Object[] getCandidateObjects(LispArgslist result) {
        return new Object[] { result };
    }

    @Override
    public void showParameterInfo(@NotNull LispList element, @NotNull CreateParameterInfoContext context) {
        context.showHint(element, element.getTextRange().getStartOffset() + 1, this);
    }

    @Override
    public @Nullable LispList findElementForUpdatingParameterInfo(@NotNull UpdateParameterInfoContext context) {
        LispList candidate = findElementAtOffset(context.getFile(), context.getOffset());
        if (candidate != null) {
            PsiElement current = context.getParameterOwner();
            if (current == null || current == candidate) return candidate;
        }
        return null;
    }

    @Override
    public void updateParameterInfo(@NotNull LispList lispList, @NotNull UpdateParameterInfoContext context) {
        context.setParameterOwner(lispList);
    }

    @Override
    public void updateUI(LispArgslist p, @NotNull ParameterInfoUIContext context) {
        context.setupUIComponentPresentation(p.getInformation(), 0, 0, false,
                false, true, context.getDefaultParameterColor());
    }

    private LispList findElementAtOffset(PsiFile file, int offset) {
        PsiElement element = file.findElementAt(offset);
        while (element == null || element instanceof PsiWhiteSpace) {
            element = file.findElementAt(offset--);
        }

        LispList l;
        if ((element instanceof LispList)) {
            l = (LispList) element;
        } else {
            l = PsiTreeUtil.getParentOfType(element, LispList.class);
        }

        if (l != null) {
            QuoteState quoteState = LispParserUtil.getQuoteState(l);
            if (quoteState == QuoteState.NO_STATE || quoteState == QuoteState.ERROR_STATE) {
                return l;
            }
        }
        return null;
    }

}

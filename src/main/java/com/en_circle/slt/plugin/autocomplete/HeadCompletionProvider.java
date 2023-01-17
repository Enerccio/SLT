package com.en_circle.slt.plugin.autocomplete;

import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.SymbolState.SymbolBinding;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.swank.requests.SimpleCompletion;
import com.en_circle.slt.tools.SltApplicationUtils;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionProvider;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.icons.AllIcons.Nodes;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class HeadCompletionProvider extends CompletionProvider<CompletionParameters> {
    @Override
    protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
        if (SltLispEnvironmentProvider.getInstance().isLispEnvironmentActive()) {
            String startedSymbol = result.getPrefixMatcher().getPrefix();
            String packageName = LispParserUtil.getPackage(parameters.getOriginalFile(), parameters.getOffset());
            List<LookupElementBuilder> builderList = SltApplicationUtils.getAsyncResultNoThrow(finishRequest -> SimpleCompletion
                    .simpleCompletion(startedSymbol, packageName, lr -> {
                        List<LookupElementBuilder> builders = new ArrayList<>();
                        try {
                            if (lr instanceof LispContainer container) {
                                if (container.getItems().get(0) instanceof LispContainer innerList) {
                                    for (LispElement element : innerList.getItems()) {
                                        if (element instanceof LispString str) {
                                            SymbolState state = SltLispEnvironmentProvider.getInstance()
                                                    .refreshSymbolFromServer(null, str.getValue(), null);
                                            LookupElementBuilder builder = LookupElementBuilder.create(str.getValue());
                                            if (state.binding == SymbolBinding.MACRO) {
                                                builder = builder.withIcon(Nodes.Template);
                                            } else if (state.binding == SymbolBinding.FUNCTION) {
                                                builder = builder.withIcon(Nodes.Function);
                                            } else if (state.binding == SymbolBinding.METHOD) {
                                                builder = builder.withIcon(Nodes.Method);
                                            } else {
                                                builder = builder.withIcon(Nodes.Lambda);
                                            }
                                            builders.add(builder);
                                        }
                                    }
                                }
                            }
                        } finally {
                            finishRequest.accept(builders);
                        }
                    }));
            if (builderList != null) {
                for (LookupElementBuilder builder : builderList) {
                    result.addElement(builder);
                }
            }
        }
    }
}

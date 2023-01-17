package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.autocomplete.HeadCompletionProvider;
import com.en_circle.slt.plugin.lisp.psi.SltPatterns;
import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionType;

public class SltCompletionContributor extends CompletionContributor {

    public SltCompletionContributor() {
        extend(CompletionType.BASIC, SltPatterns.getHeadPattern(), new HeadCompletionProvider());
    }

}

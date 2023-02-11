package com.en_circle.slt.plugin.spellcheck;

import com.en_circle.slt.plugin.lisp.psi.LispString;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.spellchecker.inspections.IdentifierSplitter;
import com.intellij.spellchecker.tokenizer.TokenConsumer;
import com.intellij.spellchecker.tokenizer.Tokenizer;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

public class LispStringTokenizer extends Tokenizer<LispString> {

    @Override
    public void tokenize(@NotNull LispString element, TokenConsumer consumer) {
        VirtualFile virtualFile = element.getContainingFile().getVirtualFile();
        ProjectFileIndex fileIndex = ProjectRootManager.getInstance(element.getProject()).getFileIndex();
        boolean isInSource = (virtualFile != null) && fileIndex.isInContent(virtualFile);
        if (isInSource) {
            String text = element.getText();
            if (StringUtils.isBlank(text))
                return;

            consumer.consumeToken(element, text, false, 0, TextRange.create(1, text.length()-1),
                    IdentifierSplitter.getInstance());
        }
    }

}

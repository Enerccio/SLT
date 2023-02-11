package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.lisp.LispLexerAdapter;
import com.en_circle.slt.plugin.lisp.LispParser;
import com.en_circle.slt.plugin.lisp.psi.LispFile;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lang.ASTNode;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import org.jetbrains.annotations.NotNull;

public class SltCommonLispParserDefinition implements ParserDefinition {

    public static final IFileElementType FILE = new IFileElementType(SltCommonLispLanguage.INSTANCE);
    public static final TokenSet STRINGS = TokenSet.create(LispTypes.STRING_TOKEN);

    public static final TokenSet COMMENTS = TokenSet.create(LispTypes.LINE_COMMENT,  LispTypes.BLOCK_COMMENT);
    public static final TokenSet SYMBOLS = TokenSet.create(LispTypes.SYMBOL_TOKEN);
    public static final TokenSet LITERALS = TokenSet.create(LispTypes.STRING_TOKEN, LispTypes.BIT_ARRAY,
            LispTypes.BINARY_NUMBER_TOKEN, LispTypes.HEX_NUMBER_TOKEN, LispTypes.RADIX_NUMBER_TOKEN,
            LispTypes.REAL_NUMBER, LispTypes.INTEGER_NUMBER, LispTypes.RATIO_NUMBER);


    @Override
    public @NotNull Lexer createLexer(Project project) {
        return new LispLexerAdapter();
    }

    @Override
    public @NotNull PsiParser createParser(Project project) {
        return new LispParser();
    }

    @Override
    public @NotNull IFileElementType getFileNodeType() {
        return FILE;
    }

    @Override
    public @NotNull TokenSet getCommentTokens() {
        return COMMENTS;
    }

    @Override
    public @NotNull TokenSet getStringLiteralElements() {
        return STRINGS;
    }

    @Override
    public @NotNull PsiElement createElement(ASTNode node) {
        return LispTypes.Factory.createElement(node);
    }

    @Override
    public @NotNull PsiFile createFile(@NotNull FileViewProvider viewProvider) {
        return new LispFile(viewProvider);
    }
}

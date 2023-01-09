package com.en_circle.slt.plugin.highlights;

import com.en_circle.slt.plugin.lisp.LispLexerAdapter;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;

public class CommonLispStaticHighlighter extends SyntaxHighlighterBase {

    private static final HashMap<IElementType, TextAttributesKey> colors = new HashMap<>();

    static {
        SyntaxHighlighterBase.fillMap(colors, CommonLispHighlighterColors.COMMENT, LispTypes.COMMENT);
        SyntaxHighlighterBase.fillMap(colors, CommonLispHighlighterColors.PARENTS, LispTypes.LPAREN, LispTypes.RPAREN);
        SyntaxHighlighterBase.fillMap(colors, CommonLispHighlighterColors.NUMBER, LispTypes.NUMBER_TOKEN);
        SyntaxHighlighterBase.fillMap(colors, CommonLispHighlighterColors.STRING, LispTypes.STRING_TOKEN);
    }

    @Override
    public @NotNull Lexer getHighlightingLexer() {
        return new LispLexerAdapter();
    }

    @Override
    public TextAttributesKey @NotNull [] getTokenHighlights(IElementType tokenType) {
        return SyntaxHighlighterBase.pack(colors.get(tokenType));
    }
}

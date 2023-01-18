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
        SyntaxHighlighterBase.fillMap(colors, CommonLispHighlighterColors.COMMENT,
                LispTypes.LINE_COMMENT, LispTypes.BLOCK_COMMENT);
        SyntaxHighlighterBase.fillMap(colors, CommonLispHighlighterColors.PARENTS, LispTypes.LPAREN, LispTypes.RPAREN);
        SyntaxHighlighterBase.fillMap(colors, CommonLispHighlighterColors.NUMBER,
                LispTypes.BIT_ARRAY, LispTypes.BINARY_NUMBER_TOKEN, LispTypes.HEX_NUMBER_TOKEN, LispTypes.RADIX_NUMBER_TOKEN,
                LispTypes.REAL_NUMBER, LispTypes.INTEGER_NUMBER, LispTypes.RATIO_NUMBER);
        SyntaxHighlighterBase.fillMap(colors, CommonLispHighlighterColors.STRING, LispTypes.STRING_TOKEN);
        SyntaxHighlighterBase.fillMap(colors, CommonLispHighlighterColors.SUGAR,
                LispTypes.FUNCTION, LispTypes.UNINTERN, LispTypes.REFERENCE_SET, LispTypes.REFERENCE_LABEL,
                LispTypes.TEST_SUCCESS, LispTypes.TEST_FALURE, LispTypes.EVAL_VALUE, LispTypes.ARRAY_START,
                LispTypes.PATHNAME_INDICATOR, LispTypes.STRUCTURE_TOKEN);
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

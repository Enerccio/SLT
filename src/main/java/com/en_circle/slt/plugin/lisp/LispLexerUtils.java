package com.en_circle.slt.plugin.lisp;

import com.en_circle.slt.plugin.lisp.number.LispNumberLexerAdapter;
import com.en_circle.slt.plugin.lisp.number.LispNumberParser;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.apache.commons.lang3.StringUtils;

public class LispLexerUtils {

    public static IElementType processToken(LispNumberLexerAdapter lexer, String token) {
        if (StringUtils.containsOnly(token, ".")) {
            if (token.length() == 1) {
                return LispTypes.DOT;
            } else {
                return TokenType.ERROR_ELEMENT;
            }
        }

        if (":".equals(token)) {
            return TokenType.ERROR_ELEMENT;
        }

        IElementType number = new LispNumberParser().parse(lexer, token);
        if (number != null) {
            return number;
        }
        return LispTypes.SYMBOL_TOKEN;
    }

}

package com.en_circle.slt.plugin.lisp;

import com.en_circle.slt.plugin.lisp.number.LispNumberLexerAdapter;
import com.en_circle.slt.plugin.lisp.number.LispNumberParser;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.apache.commons.lang3.StringUtils;

import java.util.regex.Pattern;

public class LispLexerUtils {

    // keep in sync with flex!
    public static final Pattern whitespace = Pattern.compile("[\\r\\n\\t \\x0c\\x0a]");

    public static IElementType processToken(LispNumberLexerAdapter lexer, String token) {
        if (StringUtils.containsOnly(token, ".")) {
            if (token.length() == 1) {
                return LispTypes.DOT;
            } else {
                return TokenType.ERROR_ELEMENT;
            }
        }

        if (token.endsWith(":")) {
            return TokenType.ERROR_ELEMENT;
        }

        IElementType number = new LispNumberParser().parse(lexer, token);
        if (number != null) {
            return number;
        }
        return LispTypes.SYMBOL_TOKEN;
    }

    public static boolean isWhitespace(char charAt) {
        return whitespace.matcher(String.valueOf(charAt)).matches();
    }
}

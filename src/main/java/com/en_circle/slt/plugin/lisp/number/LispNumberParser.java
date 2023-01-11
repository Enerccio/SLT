package com.en_circle.slt.plugin.lisp.number;

import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lexer.LexerPosition;
import com.intellij.psi.tree.IElementType;

public class LispNumberParser {

    public IElementType parse(LispNumberLexerAdapter lexer, String data) {
        // TODO http://www.lispworks.com/documentation/lw70/CLHS/Body/02_c.htm

        lexer.start(data);
        LexerPosition start = lexer.getCurrentPosition();

        if (integer(lexer, start)) {
            return LispTypes.INTEGER_NUMBER;
        } else if (ratio(lexer, start)) {
            return LispTypes.RATIO_NUMBER;
        } else if (floatNumber(lexer, start)) {
            return LispTypes.REAL_NUMBER;
        }
        return null;
    }

    private boolean integer(LispNumberLexerAdapter lexer, LexerPosition start) {
        lexer.restore(start);

        if (hasToken(lexer, LispNumberType.SIGN)) {
            lexer.advance();
        }

        boolean hasAtLeastOneDigit = false;
        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                hasAtLeastOneDigit = true;
                lexer.advance();
            } else if (hasToken(lexer, LispTypes.DOT)) {
                if (!hasAtLeastOneDigit) {
                    return false; // fail
                }
                lexer.advance();
                break;
            } else {
                return false; // fail
            }
        }

        if (!hasAtLeastOneDigit) {
            return false; // fail
        }

        return lexer.getTokenType() == null;
    }

    private boolean ratio(LispNumberLexerAdapter lexer, LexerPosition start) {
        lexer.restore(start);

        if (hasToken(lexer, LispNumberType.SIGN)) {
            lexer.advance();
        }

        boolean hasSlash = false;
        boolean hasAtLeastOneDigit = false;
        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                hasAtLeastOneDigit = true;
                lexer.advance();
            } else if (hasToken(lexer, LispNumberType.SLASH)) {
                if (!hasAtLeastOneDigit) {
                    return false; // fail
                }
                if (hasSlash) {
                    return false; // fail
                }
                hasSlash = true;
                hasAtLeastOneDigit = false;
                lexer.advance();
            } else {
                return false; // fail
            }
        }

        if (!hasAtLeastOneDigit || !hasSlash) {
            return false; // fail
        }

        return lexer.getTokenType() == null;
    }

    private boolean floatNumber(LispNumberLexerAdapter lexer, LexerPosition start) {
        lexer.restore(start);
        if (floatNumber1(lexer)) {
            return true;
        }

        lexer.restore(start);
        if (floatNumber2(lexer)) {
            return true;
        }
        return false;
    }

    private boolean floatNumber1(LispNumberLexerAdapter lexer) {
        if (hasToken(lexer, LispNumberType.SIGN)) {
            lexer.advance();
        }

        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                lexer.advance();
            } else {
                break;
            }
        }

        if (!hasToken(lexer, LispTypes.DOT)) {
            return false;
        }
        lexer.advance();

        boolean hasAtLeastOneDigit = false;
        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                hasAtLeastOneDigit = true;
                lexer.advance();
            } else {
                return false; // fail
            }
        }

        if (!hasAtLeastOneDigit) {
            return false;
        }

        exponent(lexer);

        return lexer.getTokenType() == null;
    }

    private boolean floatNumber2(LispNumberLexerAdapter lexer) {
        if (hasToken(lexer, LispNumberType.SIGN)) {
            lexer.advance();
        }

        boolean hasAtLeastOneDigit = false;
        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                hasAtLeastOneDigit = true;
                lexer.advance();
            } else {
                break; // fail
            }
        }

        if (!hasAtLeastOneDigit) {
            return false;
        }

        if (lexer.getTokenType() == LispTypes.DOT) {
            lexer.advance();

            while (lexer.getTokenType() != null) {
                if (hasToken(lexer, LispNumberType.DIGIT)) {
                    lexer.advance();
                } else {
                    break;
                }
            }
        }

        if (!exponent(lexer)) {
            return false;
        }

        return lexer.getTokenType() == null;
    }

    private boolean exponent(LispNumberLexerAdapter lexer) {
        if (!hasToken(lexer, LispNumberType.EXPONENT_MARKER)) {
            return false;
        }
        lexer.advance();

        if (hasToken(lexer, LispNumberType.SIGN)) {
            lexer.advance();
        }

        boolean hasAtLeastOneDigit = false;
        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                hasAtLeastOneDigit = true;
                lexer.advance();
            } else {
                return false; // fail
            }
        }

        if (!hasAtLeastOneDigit) {
            return false;
        }

        return true;
    }

    private boolean hasToken(LispNumberLexerAdapter lexer, IElementType type) {
        IElementType token = lexer.getTokenType();
        return type.equals(token);
    }
}

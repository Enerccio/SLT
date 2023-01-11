package com.en_circle.slt.plugin.lisp.number;

import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.lexer.LexerPosition;
import com.intellij.psi.tree.IElementType;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

public class LispNumberParser {

    private NumberContext ctx;

    public IElementType parse(LispNumberLexerAdapter lexer, String data) {

        lexer.start(data);
        LexerPosition start = lexer.getCurrentPosition();

        if (integer(lexer, start)) {
            return LispTypes.INTEGER_NUMBER;
        } else if (ratio(lexer, start)) {
            return LispTypes.RATIO_NUMBER;
        } else if (floatNumber(lexer, start)) {
            return LispTypes.REAL_NUMBER;
        }
        ctx = null;
        return null;
    }

    private boolean integer(LispNumberLexerAdapter lexer, LexerPosition start) {
        lexer.restore(start);
        ctx = new NumberContext();

        if (hasToken(lexer, LispNumberType.SIGN)) {
            if (lexer.getTokenSequence().equals("-")) {
                ctx.negative = true;
            }
            lexer.advance();
        }

        boolean hasAtLeastOneDigit = false;
        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                hasAtLeastOneDigit = true;
                ctx.decimal.append(lexer.getTokenSequence());
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
        ctx = new NumberContext();

        if (hasToken(lexer, LispNumberType.SIGN)) {
            if (lexer.getTokenSequence().equals("-")) {
                ctx.negative = true;
            }
            lexer.advance();
        }

        boolean hasSlash = false;
        boolean hasAtLeastOneDigit = false;
        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                hasAtLeastOneDigit = true;
                if (!hasSlash) {
                    ctx.p.append(lexer.getTokenSequence());
                } else {
                    ctx.q.append(lexer.getTokenSequence());
                }
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
        ctx = new NumberContext();

        if (floatNumber1(lexer)) {
            return true;
        }

        lexer.restore(start);
        ctx = new NumberContext();
        if (floatNumber2(lexer)) {
            return true;
        }

        return false;
    }

    private boolean floatNumber1(LispNumberLexerAdapter lexer) {
        if (hasToken(lexer, LispNumberType.SIGN)) {
            if (lexer.getTokenSequence().equals("-")) {
                ctx.negative = true;
            }
            lexer.advance();
        }

        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                ctx.beforeDot.append(lexer.getTokenSequence());
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
                ctx.afterDot.append(lexer.getTokenSequence());
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
            if (lexer.getTokenSequence().equals("-")) {
                ctx.negative = true;
            }
            lexer.advance();
        }

        boolean hasAtLeastOneDigit = false;
        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                hasAtLeastOneDigit = true;
                ctx.beforeDot.append(lexer.getTokenSequence());
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
                    ctx.afterDot.append(lexer.getTokenSequence());
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
            if (lexer.getTokenSequence().equals("-")) {
                ctx.exponentNegative = true;
            }
            lexer.advance();
        }

        boolean hasAtLeastOneDigit = false;
        while (lexer.getTokenType() != null) {
            if (hasToken(lexer, LispNumberType.DIGIT)) {
                hasAtLeastOneDigit = true;
                lexer.advance();
                ctx.exponentValue.append(lexer.getTokenSequence());
            } else {
                return false; // fail
            }
        }

        if (!hasAtLeastOneDigit) {
            return false;
        }

        ctx.hasExponent = true;
        return true;
    }

    private boolean hasToken(LispNumberLexerAdapter lexer, IElementType type) {
        IElementType token = lexer.getTokenType();
        return type.equals(token);
    }

    public BigInteger getAsBigInteger(IElementType parsed) {
        if (LispTypes.INTEGER_NUMBER.equals(parsed)) {
            BigInteger i = new BigInteger(ctx.decimal.toString());
            if (ctx.negative) {
                return i.negate();
            }
            return i;
        }
        return null;
    }

    public BigDecimal getAsBigDecimal(IElementType parsed) {
        if (LispTypes.RATIO_NUMBER.equals(parsed)) {
            BigDecimal d = new BigDecimal(ctx.p.toString());
            d = d.divide(new BigDecimal(ctx.q.toString()), RoundingMode.HALF_UP);
            if (ctx.negative) {
                return d.negate();
            }
            return d;
        } else if (LispTypes.REAL_NUMBER.equals(parsed)) {
            String realNumPart = ctx.beforeDot.toString();
            String realAD = ctx.afterDot.toString();
            if (StringUtils.isNotBlank(realAD)) {
                realNumPart += "." + realAD;
            }
            BigDecimal d = new BigDecimal(realNumPart);

            if (ctx.hasExponent) {
                BigDecimal exponent = new BigDecimal(ctx.exponentValue.toString());
                if (ctx.exponentNegative)
                    exponent = exponent.negate();
                d = d.multiply(BigDecimal.TEN.multiply(exponent));
            }
            if (ctx.negative) {
                return d.negate();
            }
            return d;
        }
        return null;
    }

    public NumberContext getCtx() {
        return ctx;
    }

    public static class NumberContext {

        public boolean negative = false;
        public StringBuffer decimal = new StringBuffer();
        public StringBuffer p = new StringBuffer();
        public StringBuffer q = new StringBuffer();
        public StringBuffer beforeDot = new StringBuffer();
        public StringBuffer afterDot = new StringBuffer();

        public boolean hasExponent = false;
        public boolean exponentNegative = false;
        public StringBuffer exponentValue = new StringBuffer();

    }

}

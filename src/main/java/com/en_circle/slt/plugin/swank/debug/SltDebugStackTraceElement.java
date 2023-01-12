package com.en_circle.slt.plugin.swank.debug;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;

public class SltDebugStackTraceElement {

    private final String line;

    public SltDebugStackTraceElement(LispContainer source) {
        String text = ((LispString) source.getItems().get(1)).getValue();
        this.line = LispUtils.unescape(text);
    }

    public String getLine() {
        return line;
    }

    @Override
    public String toString() {
        return "SltDebugStackTraceElement{" +
                "line='" + line + '\'' +
                '}';
    }
}

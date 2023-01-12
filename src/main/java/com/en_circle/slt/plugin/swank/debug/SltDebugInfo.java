package com.en_circle.slt.plugin.swank.debug;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispInteger;
import com.en_circle.slt.plugin.lisp.lisp.LispString;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class SltDebugInfo {

    private final LispContainer source;
    private BigInteger threadId;
    private BigInteger debugLevel;
    private String info;
    private String conditionInfo;
    private List<SltDebugAction> actions;
    private List<SltDebugStackTraceElement> stacktrace;

    public SltDebugInfo(LispContainer source) {
        this.source = source;
        parse(source);
    }

    private void parse(LispContainer source) {
        threadId = ((LispInteger) source.getItems().get(1)).getValue();
        debugLevel = ((LispInteger) source.getItems().get(2)).getValue();

        parseInfo((LispContainer) source.getItems().get(3));
        parseRestarts((LispContainer) source.getItems().get(4));
        parseStackTrace((LispContainer) source.getItems().get(5));
    }

    private void parseInfo(LispContainer info) {
        this.info = ((LispString) info.getItems().get(0)).getValue();
        this.conditionInfo = ((LispString) info.getItems().get(1)).getValue();
    }

    private void parseRestarts(LispContainer restarts) {
        actions = new ArrayList<>();
        for (LispElement element : restarts.getItems()) {
            actions.add(new SltDebugAction((LispContainer) element));
        }
    }

    private void parseStackTrace(LispContainer stackTrace) {
        stacktrace = new ArrayList<>();
        for (LispElement element : stackTrace.getItems()) {
            stacktrace.add(new SltDebugStackTraceElement((LispContainer) element));
        }
    }

    public LispContainer getSource() {
        return source;
    }

    public BigInteger getThreadId() {
        return threadId;
    }

    public BigInteger getDebugLevel() {
        return debugLevel;
    }

    public String getInfo() {
        return info;
    }

    public String getConditionInfo() {
        return conditionInfo;
    }

    public List<SltDebugAction> getActions() {
        return actions;
    }

    public List<SltDebugStackTraceElement> getStacktrace() {
        return stacktrace;
    }

    @Override
    public String toString() {
        return "SltDebugInfo{" +
                "debugId=" + threadId +
                ", debugCommandId=" + debugLevel +
                ", info='" + info + '\'' +
                ", conditionInfo='" + conditionInfo + '\'' +
                ", actions=" + actions +
                ", stacktrace=" + stacktrace +
                '}';
    }
}

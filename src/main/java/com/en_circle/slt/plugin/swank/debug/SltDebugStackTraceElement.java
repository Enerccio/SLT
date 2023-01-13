package com.en_circle.slt.plugin.swank.debug;

import com.en_circle.slt.plugin.lisp.lisp.*;

public class SltDebugStackTraceElement {

    private final String line;
    private final String detailedCall;
    private String framePackage = "cl-user";
    private String location = "undefined";
    private int position = -1;
    private boolean isPrecise = false;
    private boolean isFile = false;

    public SltDebugStackTraceElement(LispContainer source) {
        String text = source.getItems().get(2).toString();
        this.line = LispUtils.unescape(text);
        text = source.getItems().get(1).toString();
        this.detailedCall = LispUtils.unescape(text);

        try {
            LispContainer src = (LispContainer) source.getItems().get(3);
            if (src.getItems().get(0).equals(new LispSymbol(":location"))) {
                for (int i=1; i<src.getItems().size(); i++) {
                    LispContainer trait = (LispContainer) src.getItems().get(i);
                    if (LispUtils.hasPValue(trait, new LispSymbol(":file"))) {
                        this.location = ((LispString) LispUtils.pvalue(trait, new LispSymbol(":file"))).getValue();
                        isFile = true;
                    } else if (LispUtils.hasPValue(trait, new LispSymbol(":buffer"))) {
                        this.location = ((LispString) LispUtils.pvalue(trait, new LispSymbol(":buffer"))).getValue();
                        isFile = true;
                    } else if (LispUtils.hasPValue(trait, new LispSymbol(":position"))) {
                        LispInteger offset = (LispInteger)  LispUtils.pvalue(trait, new LispSymbol(":position"));
                        position = offset.getValue().intValue();
                    } else if (LispUtils.hasPValue(trait, new LispSymbol(":offset"))) {
                        LispInteger offset = (LispInteger) LispUtils.pvalue(trait, new LispSymbol(":offset"));
                        position = offset.getValue().intValue();
                    }
                }
            }
            if (isFile) {
                isPrecise = true;
            }
        } catch (Exception ignored) {

        }

        LispElement packageInfo = source.getItems().get(4);
        if (packageInfo instanceof LispString) {
            framePackage = ((LispString) packageInfo).getValue();
        }
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

    public String getLocation() {
        return location;
    }

    public int getPosition() {
        return position;
    }

    public boolean isPrecise() {
        return isPrecise;
    }

    public boolean isFile() {
        return isFile;
    }

    public String getDetailedCall() {
        return detailedCall;
    }

    public String getFramePackage() {
        return framePackage;
    }
}

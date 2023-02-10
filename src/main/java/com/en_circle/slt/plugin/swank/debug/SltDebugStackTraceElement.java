package com.en_circle.slt.plugin.swank.debug;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.swank.components.SourceLocation;

public class SltDebugStackTraceElement {

    private final String line;
    private final String detailedCall;
    private String framePackage = "cl-user";
    private final SourceLocation location;

    public SltDebugStackTraceElement(LispContainer source) {
        String text = source.getItems().get(2).nativeString();
        this.line = LispUtils.unescape(text);
        text = source.getItems().get(1).toString();
        this.detailedCall = LispUtils.unescape(text);
        this.location = new SourceLocation(source.getItems().get(3));
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
        return location.getLocation();
    }

    public int getPosition() {
        return location.getPosition();
    }

    public boolean isPrecise() {
        return location.isPrecise();
    }

    public boolean isFile() {
        return location.isFile();
    }

    public String getDetailedCall() {
        return detailedCall;
    }

    public String getFramePackage() {
        return framePackage;
    }
}

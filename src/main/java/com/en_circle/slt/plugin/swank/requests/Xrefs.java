package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.intellij.openapi.project.Project;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Xrefs extends SlimeRequest {

    public static SlimeRequest xrefs(String name, String packageName, Callback callback, XrefType... xrefTypes) {
        return new Xrefs(name, packageName, callback, xrefTypes);
    }

    public static SlimeRequest xrefs(String name, String packageName, List<XrefType> xrefTypes, Callback callback) {
        return new Xrefs(name, packageName, xrefTypes, callback);
    }

    private final String name;
    private final String packageName;
    private final List<XrefType> xrefTypeList;
    private final Callback callback;

    private Xrefs(String name, String packageName, Callback callback, XrefType... xrefTypes) {
        this(name, packageName, Arrays.asList(xrefTypes), callback);
    }

    private Xrefs(String name, String packageName, List<XrefType> xrefTypes, Callback callback) {
        this.name = name;
        this.packageName = packageName;
        this.xrefTypeList = xrefTypes;
        this.callback = callback;
    }

    public void processReply(LispContainer data) {
        if (isOk(data)) {
            callback.onResult(data.getItems().get(1));
        }
    }

    private boolean isOk(LispContainer data) {
        return data.getItems().size() > 0 &&
                data.getItems().get(0) instanceof LispSymbol &&
                ":ok".equals(((LispSymbol) data.getItems().get(0)).getValue());
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId, Project project) {
        return SwankPacket.xrefs(xrefTypeList.stream().map(XrefType::getName).collect(Collectors.joining(" ")),
                name, packageName, requestId);
    }

    public interface Callback {
        void onResult(LispElement result);
    }

    public enum XrefType {

        CALLS(":calls"),
        @Deprecated
        CALLS_WHO(":calls-who"),
        REFERENCES(":references"),
        BINDS(":binds"),
        @Deprecated
        SETS(":sets"),
        MACRO_EXPANDS(":macroexpands"),
        SPECIALIZES(":specializes"),
        @Deprecated
        CALLERS(":callers"),
        @Deprecated
        CALLEES(":callees")

        ;

        private final String name;

        XrefType(String name) {
            this.name = name;
        }

        public String getName() {
            return this.name;
        }

    }

}

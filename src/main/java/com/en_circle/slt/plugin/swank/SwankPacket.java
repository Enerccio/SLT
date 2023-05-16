package com.en_circle.slt.plugin.swank;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;

public class SwankPacket {

    public static SwankPacket writeString(String value) {
        value = StringUtils.replace(value, "\\", "\\\\");
        value = StringUtils.replace(value, "\"", "\\\"");
        String formatted = String.format("(:write-string '\"%s\n\")", value);
        return new SwankPacket(formatted);
    }

    public static SwankPacket rpcReturnOk(String sexpression, int continuation) {
        String formatted = String.format("(:return (:ok '%s) %s)", sexpression, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket rpcReturnAbort(String sexpression, int continuation) {
        String formatted = String.format("(:return (:abort '%s) %s)", sexpression, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket rpcNewPackage(String newPackage) {
        newPackage = StringUtils.replace(newPackage, "\\", "\\\\");
        newPackage = StringUtils.replace(newPackage, "\"", "\\\"");
        String formatted = String.format("(:new-package \"%s\" \"%s\")", newPackage, newPackage);
        return new SwankPacket(formatted);
    }

    public static SwankPacket rpcWriteString(String sexpression) {
        String formatted = String.format("(:write-string '\"%s\n\")", sexpression);
        return new SwankPacket(formatted);
    }

    public static SwankPacket sltEval(String sexpression, String breakpoints, BigInteger continuation) {
        return sltEval(sexpression, breakpoints, ":CL-USER", continuation);
    }

    public static SwankPacket sltEval(String sexpression, String breakpoints, String packageName, BigInteger continuation) {
        return sltEval(sexpression, breakpoints, packageName, "T", continuation);
    }

    public static SwankPacket sltEval(String sexpression, String breakpoints,
                                      String packageName, String thread, BigInteger continuation) {
        String evalExpression = sexpression;
        if (breakpoints != null) {
            evalExpression = String.format("(progn (slt-core::with-breakpoints \"%s\") %s)", breakpoints, sexpression);
        }
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        evalExpression = StringUtils.replace(evalExpression, "\\", "\\\\");
        evalExpression = StringUtils.replace(evalExpression, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:slt-eval \"%s\") \":%s\" %s %s)",
                evalExpression, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket evalInFrame(String sexpression, String breakpoints, BigInteger frame, String packageName, BigInteger thread, BigInteger continuation) {
        String evalExpression = sexpression;
        if (breakpoints != null) {
            evalExpression = String.format("(progn (slt-core::with-breakpoints \"%s\") %s)", breakpoints, sexpression);
        }
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        evalExpression = StringUtils.replace(evalExpression, "\\", "\\\\");
        evalExpression = StringUtils.replace(evalExpression, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:eval-string-in-frame \"%s\" %s \"%s\") \":%s\" %s %s)",
                evalExpression, frame, packageName, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket evalRegion(String region, String breakpoints, BigInteger continuation) {
        return evalRegion(region, breakpoints, ":CL-USER", "T", continuation);
    }

    public static SwankPacket evalRegion(String region, String breakpoints, String packageName, BigInteger continuation) {
        return evalRegion(region, breakpoints, packageName, "T", continuation);
    }

    public static SwankPacket evalRegion(String region, String breakpoints, String packageName, String thread, BigInteger continuation) {
        String evalRegion = region;
        if (breakpoints != null) {
            evalRegion = String.format("(progn (slt-core::with-breakpoints \"%s\") %s)", breakpoints, region);
        }
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        evalRegion = StringUtils.replace(evalRegion, "\\", "\\\\");
        evalRegion = StringUtils.replace(evalRegion, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:interactive-eval-region \"%s\") \":%s\" %s %s)",
                evalRegion, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket swankEvalAndGrab(String sexpression, String breakpoints, BigInteger continuation) {
        return swankEvalAndGrab(sexpression, breakpoints, ":CL-USER", continuation);
    }

    public static SwankPacket swankEvalAndGrab(String sexpression, String breakpoints, String packageName, BigInteger continuation) {
        return swankEvalAndGrab(sexpression, breakpoints, packageName, "T", continuation);
    }

    public static SwankPacket swankEvalAndGrab(String sexpression, String breakpoints, String packageName, String thread, BigInteger continuation) {
        String evalExpression = sexpression;
        if (breakpoints != null) {
            evalExpression = String.format("(progn (slt-core::with-breakpoints \"%s\") %s)", breakpoints, sexpression);
        }
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        evalExpression = StringUtils.replace(evalExpression, "\\", "\\\\");
        evalExpression = StringUtils.replace(evalExpression, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:eval-and-grab-output \"%s\") \":%s\" %s %s)",
                evalExpression, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket swankEvalRegion(String code, String breakpoints, String filename, int bufferPosition, BigInteger continuation) {
        return swankEvalRegion(code, breakpoints, filename, bufferPosition, ":CL-USER", continuation);
    }

    public static SwankPacket swankEvalRegion(String code, String breakpoints, String filename, int bufferPosition, String packageName, BigInteger continuation) {
        return swankEvalRegion(code, breakpoints, filename, bufferPosition, packageName, "T", continuation);
    }

    public static SwankPacket swankEvalRegion(String code, String breakpoints, String filename, int bufferPosition,
                                              String packageName, String thread, BigInteger continuation) {
        if (breakpoints == null) {
            breakpoints = "NIL";
        } else {
            breakpoints = "\"" + breakpoints + "\"";
        }
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        code = StringUtils.replace(code, "\\", "\\\\");
        code = StringUtils.replace(code, "\"", "\\\"");
        breakpoints = StringUtils.replace(breakpoints, "\\", "\\\\");
        breakpoints = StringUtils.replace(breakpoints, "\"", "\\\"");
        filename = StringUtils.replace(filename, "\\", "\\\\");
        filename = StringUtils.replace(filename, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:compile-string-region-slt \"%s\" %s \"%s\" %s \"%s\" :%s) \":%s\" %s %s)",
                code, breakpoints, filename, bufferPosition, filename, packageName, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket invokeNthRestart(BigInteger option, BigInteger level,
                                               String restartArg, String restartArgs, BigInteger threadId, BigInteger continuation) {
        restartArg = StringUtils.replace(restartArg, "\\", "\\\\");
        restartArg = StringUtils.replace(restartArg, "\"", "\\\"");
        restartArgs = StringUtils.replace(restartArgs, "\\", "\\\\");
        restartArgs = StringUtils.replace(restartArgs, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:invoke-nth-restart-slt '%s '%s \"%s\" \"%s\") \":CL-USER\" %s %s)",
                level, option, restartArg, restartArgs, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket throwToToplevel(BigInteger threadId, BigInteger continuation) {
        String formatted = String.format("(:emacs-rex (swank:throw-to-toplevel) :CL-USER %s %s)", threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket frameLocals(BigInteger frame, BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:frame-locals-and-catch-tags %s) \":%s\" %s %s)",
                frame, packageName, threadId == null ? "T" : threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket inspectLocal(BigInteger ix, BigInteger frameId, BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspect-frame-var %s %s) \":%s\" %s %s)",
                frameId, ix, packageName, threadId == null ? "T" : threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket frameInspectNth(BigInteger ix, BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspect-nth-part %s) \":%s\" %s %s)",
                ix, packageName, threadId == null ? "T" : threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket inspectorBack(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspector-pop) \":%s\" %s %s)",
                packageName, threadId == null ? "T" : threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket inspectorForward(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspector-next) \":%s\" %s %s)",
                packageName, threadId == null ? "T" : threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket inspectorRefresh(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspector-reinspect) \":%s\" %s %s)",
                packageName, threadId == null ? "T" : threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket inspectSymbol(String symbol, BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        symbol = StringUtils.replace(symbol, "\\", "\\\\");
        symbol = StringUtils.replace(symbol, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:init-inspector \"%s\") \":%s\" %s %s)",
                symbol, packageName, threadId == null ? "T" : threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket activateStepping(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank::activate-stepping 0) \":%s\" %s %s)",
                packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket stepperIn(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:sldb-step 0) \":%s\" %s %s)",
                packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket stepperOut(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:sldb-out 0) \":%s\" %s %s)",
                packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket stepperNext(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:sldb-next 0) \":%s\" %s %s)",
                packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket loadFile(String file, String breakpoints, BigInteger continuation) {
        return loadFile(file, breakpoints, "CL-USER", continuation);
    }

    public static SwankPacket loadFile(String file, String breakpoints, String packageName, BigInteger continuation) {
        return loadFile(file, breakpoints, packageName, "T", continuation);
    }

    public static SwankPacket loadFile(String file, String breakpoints, String packageName, String thread, BigInteger continuation) {
        if (breakpoints == null) {
            breakpoints = "NIL";
        } else {
            breakpoints = "\"" + breakpoints + "\"";
        }
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        file = StringUtils.replace(file, "\\", "\\\\");
        file = StringUtils.replace(file, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:load-file-breakpoints \"%s\" %s) \":%s\" %s %s)",
                file, breakpoints, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket macroexpand1(String form, String packageName, BigInteger continuation) {
        return macroexpand1(form, "T", packageName, continuation);
    }

    public static SwankPacket macroexpand1(String form, String threadId, String packageName, BigInteger continuation) {
        form = StringUtils.replace(form, "\\", "\\\\");
        form = StringUtils.replace(form, "\"", "\\\"");
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:swank-macroexpand-1 \"%s\") \":%s\" %s %s)",
                form, packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket macroexpand(String form, String packageName, BigInteger continuation) {
        return macroexpand(form, "T", packageName, continuation);
    }

    public static SwankPacket macroexpand(String form, String threadId, String packageName, BigInteger continuation) {
        form = StringUtils.replace(form, "\\", "\\\\");
        form = StringUtils.replace(form, "\"", "\\\"");
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:swank-macroexpand \"%s\") \":%s\" %s %s)",
                form, packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket macroexpandAll(String form, String packageName, BigInteger continuation) {
        return macroexpandAll(form, "T", packageName, continuation);
    }

    public static SwankPacket macroexpandAll(String form, String threadId, String packageName, BigInteger continuation) {
        form = StringUtils.replace(form, "\\", "\\\\");
        form = StringUtils.replace(form, "\"", "\\\"");
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:swank-macroexpand-all \"%s\") \":%s\" %s %s)",
                form, packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket simpleCompletion(String prefix, String packageName, String requestPackageName, BigInteger continuation) {
        return simpleCompletion(prefix, packageName, "T", requestPackageName, continuation);
    }

    public static SwankPacket simpleCompletion(String prefix, String packageName, String threadId, String requestPackageName, BigInteger continuation) {
        requestPackageName = StringUtils.replace(requestPackageName, "\\", "\\\\");
        requestPackageName = StringUtils.replace(requestPackageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:simple-completions \"%s\" \"%s\") \":%s\" %s %s)",
                prefix, packageName, requestPackageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket xrefs(String types, String name, String packageName, BigInteger continuation) {
        return xrefs(types, name, "T", packageName, continuation);
    }

    public static SwankPacket xrefs(String types, String name, String threadId, String requestPackageName, BigInteger continuation) {
        name = StringUtils.replace(name, "\\", "\\\\");
        name = StringUtils.replace(name, "\"", "\\\"");
        requestPackageName = StringUtils.replace(requestPackageName, "\\", "\\\\");
        requestPackageName = StringUtils.replace(requestPackageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:xrefs '(%s) \"%s\") \":%s\" %s %s)",
                types, name, requestPackageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket completeSearch(String prefix, String type, BigInteger continuation) {
        return completeSearch(prefix, type, ":CL-USER", continuation);
    }

    public static SwankPacket completeSearch(String prefix, String type, String packageName, BigInteger continuation) {
        return completeSearch(prefix, type, "T", packageName, continuation);
    }

    public static SwankPacket completeSearch(String prefix, String type, String threadId, String packageName, BigInteger continuation) {
        prefix = StringUtils.replace(prefix, "\\", "\\\\");
        prefix = StringUtils.replace(prefix, "\"", "\\\"");
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:find-reference-prefix \"%s\" %s) \":%s\" %s %s)",
                prefix, type, packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket dumpThreads(BigInteger continuation) {
        String formatted = String.format("(:emacs-rex (swank:list-threads) \":CL-USER\" T %s)", continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket breakThread(BigInteger thread, BigInteger continuation) {
        String formatted = String.format("(:emacs-rex (swank:debug-nth-thread %s) \":CL-USER\" T %s)", thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket killThread(BigInteger thread, BigInteger continuation) {
        String formatted = String.format("(:emacs-rex (swank:kill-nth-thread %s) \":CL-USER\" T %s)", thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket argslist(String symbol, String packageName, BigInteger continuation) {
        symbol = StringUtils.replace(symbol, "\\", "\\\\");
        symbol = StringUtils.replace(symbol, "\"", "\\\"");
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");

        String formatted = String.format("(:emacs-rex (swank:operator-arglist \"%s\" \"%s\") \":CL-USER\" T %s)", symbol, packageName, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket disassemble(String symbol, String packageName, BigInteger continuation) {
        symbol = StringUtils.replace(symbol, "\\", "\\\\");
        symbol = StringUtils.replace(symbol, "\"", "\\\"");
//        packageName = StringUtils.replace(packageName, "\\", "\\\\");
//        packageName = StringUtils.replace(packageName, "\"", "\\\"");

        String formatted = String.format("(:emacs-rex (swank:disassemble-form \"'%s\") \":CL-USER\" T %s)", symbol, continuation);
        return new SwankPacket(formatted);
    }

    private int length;
    private String expressionSource;

    public static SwankPacket fromInput(InputStream is) throws Exception {
        byte[] header = new byte[6];
        IOUtils.readFully(is, header);
        int length = Integer.parseInt(new String(header, StandardCharsets.UTF_8), 16);
        byte[] data = new byte[length];
        IOUtils.readFully(is, data);
        SwankPacket packet = new SwankPacket();
        packet.length = length;
        packet.expressionSource = new String(data, StandardCharsets.UTF_8);
        return packet;
    }

    public SwankPacket(String expressionSource) {
        this.expressionSource = expressionSource;
        this.length = expressionSource.getBytes(StandardCharsets.UTF_8).length;
    }

    private SwankPacket() {

    }

    public void writeTo(OutputStream inputStream) throws Exception {
        String length = String.format("%06X", this.length+1);
        IOUtils.write(length.getBytes(StandardCharsets.UTF_8), inputStream);
        IOUtils.write(expressionSource.getBytes(StandardCharsets.UTF_8), inputStream);
        IOUtils.write("\n".getBytes(StandardCharsets.UTF_8), inputStream);
    }

    @Override
    public String toString() {
        return "SlimePacket{" +
                "length=" + length +
                ", expressionSource='" + expressionSource + '\'' +
                '}';
    }

    public String getSentData() {
        return expressionSource;
    }
}

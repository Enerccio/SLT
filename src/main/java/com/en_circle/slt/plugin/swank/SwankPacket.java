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

    public static SwankPacket sltEval(String sexpression, BigInteger continuation) {
        return sltEval(sexpression, ":CL-USER", continuation);
    }

    public static SwankPacket sltEval(String sexpression, String packageName, BigInteger continuation) {
        return sltEval(sexpression, packageName, "T", continuation);
    }

    public static SwankPacket sltEval(String sexpression, String packageName, String thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        sexpression = StringUtils.replace(sexpression, "\\", "\\\\");
        sexpression = StringUtils.replace(sexpression, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:slt-eval \"%s\") \":%s\" %s %s)",
                sexpression, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket evalInFrame(String sexpression, BigInteger frame, String packageName, BigInteger thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        sexpression = StringUtils.replace(sexpression, "\\", "\\\\");
        sexpression = StringUtils.replace(sexpression, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:eval-string-in-frame \"%s\" %s \"%s\") \":%s\" %s %s)",
                sexpression, frame, packageName, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket evalRegion(String region, BigInteger continuation) {
        return evalRegion(region, ":CL-USER", "T", continuation);
    }

    public static SwankPacket evalRegion(String region, String packageName, BigInteger continuation) {
        return evalRegion(region, packageName, "T", continuation);
    }

    public static SwankPacket evalRegion(String region, String packageName, String thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        region = StringUtils.replace(region, "\\", "\\\\");
        region = StringUtils.replace(region, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:interactive-eval-region \"%s\") \":%s\" %s %s)",
                region, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket swankEvalAndGrab(String sexpression, BigInteger continuation) {
        return swankEvalAndGrab(sexpression, ":CL-USER", continuation);
    }

    public static SwankPacket swankEvalAndGrab(String sexpression, String packageName, BigInteger continuation) {
        return swankEvalAndGrab(sexpression, packageName, "T", continuation);
    }

    public static SwankPacket swankEvalAndGrab(String sexpression, String packageName, String thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        sexpression = StringUtils.replace(sexpression, "\\", "\\\\");
        sexpression = StringUtils.replace(sexpression, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:eval-and-grab-output \"%s\") \":%s\" %s %s)",
                sexpression, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket swankEvalRegion(String code, String filename, int bufferPosition, BigInteger continuation) {
        return swankEvalRegion(code, filename, bufferPosition, ":CL-USER", continuation);
    }

    public static SwankPacket swankEvalRegion(String code, String filename, int bufferPosition, String packageName, BigInteger continuation) {
        return swankEvalRegion(code, filename, bufferPosition, packageName, "T", continuation);
    }

    public static SwankPacket swankEvalRegion(String code, String filename, int bufferPosition,
                                              String packageName, String thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        code = StringUtils.replace(code, "\\", "\\\\");
        code = StringUtils.replace(code, "\"", "\\\"");
        filename = StringUtils.replace(filename, "\\", "\\\\");
        filename = StringUtils.replace(filename, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:compile-string-region-slt \"%s\" \"%s\" %s \"%s\" :%s) \":%s\" %s %s)",
                code, filename, bufferPosition, filename, packageName, packageName, thread, continuation);
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
                frame, packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket inspectLocal(BigInteger ix, BigInteger frameId, BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspect-frame-var %s %s) \":%s\" %s %s)",
                frameId, ix, packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket frameInspectNth(BigInteger ix, BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspect-nth-part %s) \":%s\" %s %s)",
                ix, packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket inspectorBack(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspector-pop) \":%s\" %s %s)",
                packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket inspectorForward(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspector-next) \":%s\" %s %s)",
                packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket inspectorRefresh(BigInteger threadId, String packageName, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:inspector-reinspect) \":%s\" %s %s)",
                packageName, threadId, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket loadFile(String file, BigInteger continuation) {
        return loadFile(file, "CL-USER", continuation);
    }

    public static SwankPacket loadFile(String file, String packageName, BigInteger continuation) {
        return loadFile(file, packageName, "T", continuation);
    }

    public static SwankPacket loadFile(String file, String packageName, String thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        file = StringUtils.replace(file, "\\", "\\\\");
        file = StringUtils.replace(file, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:load-file \"%s\") \":%s\" %s %s)",
                file, packageName, thread, continuation);
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

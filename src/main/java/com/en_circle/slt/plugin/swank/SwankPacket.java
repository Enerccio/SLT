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
        return sltEval(sexpression, "cl-user", continuation);
    }

    public static SwankPacket sltEval(String sexpression, String packageName, BigInteger continuation) {
        return sltEval(sexpression, packageName, "T", continuation);
    }

    public static SwankPacket sltEval(String sexpression, String packageName, String thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        sexpression = StringUtils.replace(sexpression, "\\", "\\\\");
        sexpression = StringUtils.replace(sexpression, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:slt-eval \"%s\") \"%s\" %s %s)",
                sexpression, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket evalRegion(String region, BigInteger continuation) {
        return evalRegion(region, "cl-user", "T", continuation);
    }

    public static SwankPacket evalRegion(String region, String packageName, BigInteger continuation) {
        return evalRegion(region, packageName, "T", continuation);
    }

    public static SwankPacket evalRegion(String region, String packageName, String thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        region = StringUtils.replace(region, "\\", "\\\\");
        region = StringUtils.replace(region, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:interactive-eval-region \"%s\") \"%s\" %s %s)",
                region, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket swankEvalAndGrab(String sexpression, BigInteger continuation) {
        return swankEvalAndGrab(sexpression, "cl-user", continuation);
    }

    public static SwankPacket swankEvalAndGrab(String sexpression, String packageName, BigInteger continuation) {
        return swankEvalAndGrab(sexpression, packageName, "T", continuation);
    }

    public static SwankPacket swankEvalAndGrab(String sexpression, String packageName, String thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        sexpression = StringUtils.replace(sexpression, "\\", "\\\\");
        sexpression = StringUtils.replace(sexpression, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:eval-and-grab-output \"%s\") \"%s\" %s %s)",
                sexpression, packageName, thread, continuation);
        return new SwankPacket(formatted);
    }

    public static SwankPacket swankSltEvalRegion(String code, String filename, int lineno, int charno, BigInteger continuation) {
        return swankSltEvalRegion(code, filename, lineno, charno, "cl-user", continuation);
    }

    public static SwankPacket swankSltEvalRegion(String code, String filename, int lineno, int charno, String packageName, BigInteger continuation) {
        return swankSltEvalRegion(code, filename, lineno, charno, packageName, "T", continuation);
    }

    public static SwankPacket swankSltEvalRegion(String code, String filename, int lineno, int charno, String packageName, String thread, BigInteger continuation) {
        packageName = StringUtils.replace(packageName, "\\", "\\\\");
        packageName = StringUtils.replace(packageName, "\"", "\\\"");
        code = StringUtils.replace(code, "\\", "\\\\");
        code = StringUtils.replace(code, "\"", "\\\"");
        filename = StringUtils.replace(filename, "\\", "\\\\");
        filename = StringUtils.replace(filename, "\"", "\\\"");
        String formatted = String.format("(:emacs-rex (swank:slt-compile-region \"%s\" \"%s\" %s %s) \"%s\" %s %s)",
                code, filename, lineno, charno, packageName, thread, continuation);
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

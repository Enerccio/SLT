package com.en_circle.slt.tools;

import com.intellij.execution.configurations.CommandLineTokenizer;

import java.io.File;
import java.util.List;
import java.util.StringTokenizer;

public class SltUtils {

    public static void addExecutable(List<String> parameters, String executable) {
        File file = new File(executable);
        if (file.getParentFile() != null) {
            String exec = file.getName();
            String path = file.getParent();
            addExecutable(parameters, exec, path);
        } else {
            addExecutable(parameters, executable, null);
        }
    }

    public static void addExecutable(List<String> parameters, String exec, String path) {
        StringTokenizer tokenizer = new CommandLineTokenizer(exec);
        boolean first = true;
        while (tokenizer.hasMoreTokens()) {
            String part = tokenizer.nextToken();
            if (path != null && first) {
                parameters.add(new File(new File(path), part).getPath());
                first = false;
            } else {
                parameters.add(part);
            }
        }
    }

}

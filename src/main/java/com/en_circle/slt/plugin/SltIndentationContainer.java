package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispInteger;
import com.en_circle.slt.plugin.lisp.lisp.LispString;

import java.math.BigInteger;
import java.util.*;

public class SltIndentationContainer {

    static final SltIndentationContainer INSTANCE = new SltIndentationContainer();

    private Map<String, IndentationUpdate> indentations = new HashMap<>();

    public synchronized void update(LispContainer updates) {
        for (LispElement element : updates.getItems()) {
            LispContainer macro = (LispContainer) element;
            LispString symbolNameElement = (LispString) macro.getItems().get(0);
            String symbolName = symbolNameElement.getValue();
            IndentationUpdate update = indentations.computeIfAbsent(symbolName, s -> new IndentationUpdate());
            update.name = symbolName.toUpperCase(Locale.ROOT);
            update.bodyArg = (macro.getItems().get(1) instanceof LispInteger) ? ((LispInteger) macro.getItems().get(1)).getValue() : null;
            update.packages.clear();
            if (macro.getItems().size() > 2 && macro.getItems().get(2) instanceof LispContainer packages) {
                for (LispElement packageNameValue : packages.getItems()) {
                    if (packageNameValue instanceof LispString packageName) {
                        update.packages.add(packageName.getValue());
                    }
                }
            }
        }
    }

    public synchronized void clear() {
        indentations.clear();
    }

    private static class IndentationUpdate {

        private String name;
        private BigInteger bodyArg;
        private final Set<String> packages = new HashSet<>();

    }

}

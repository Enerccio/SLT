package com.en_circle.slt.plugin.services.lisp.components;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.indentation.SltIndentationSettings;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.en_circle.slt.templates.Indentation;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.util.PsiTreeUtil;
import org.apache.commons.lang3.StringUtils;

import java.math.BigInteger;
import java.util.*;
import java.util.stream.Collectors;

public class SltIndentationContainer {

    private Project project;
    private final Map<String, IndentationUpdate> swankReportedIndentations = new HashMap<>();
    private final Map<String, ManualIndentation> defaultIndentations = new HashMap<>();

    public void init(Project project) {
        this.project = project;

        String data = new Indentation().render();
        PsiFileFactory factory = PsiFileFactory.getInstance(project);
        PsiFile source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        loadBaseIndentation(LispUtils.convertAst(source));
    }

    public synchronized void update(LispContainer updates) {
        for (LispElement element : updates.getItems()) {
            LispContainer macro = (LispContainer) element;
            LispString symbolNameElement = (LispString) macro.getItems().get(0);
            String symbolName = symbolNameElement.getValue();
            IndentationUpdate update = swankReportedIndentations.computeIfAbsent(symbolName, s -> new IndentationUpdate());
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
        swankReportedIndentations.clear();
    }

    private void loadBaseIndentation(List<LispElement> definitions) {
        for (LispElement lispElement : definitions) {
            LispContainer definition = (LispContainer) lispElement;
            LispSymbol form = (LispSymbol) definition.getItems().get(0);

            String formName = form.getValue().toUpperCase();
            ManualIndentation indentation = new ManualIndentation();
            defaultIndentations.put(formName, indentation);

            if (definition.getItems().get(1) instanceof LispInteger) {
                indentation.argCount = ((LispInteger) definition.getItems().get(1)).getValue().intValue();
            } else {
                LispContainer definitionArg = (LispContainer) definition.getItems().get(1);
                if (definitionArg.getItems().get(0) instanceof LispSymbol && ((LispSymbol) definitionArg.getItems().get(0)).getValue().equalsIgnoreCase("as")) {
                    indentation.ref = ((LispSymbol) definitionArg.getItems().get(1)).getValue().toUpperCase();
                } else {
                    indentation.indentationSetup = new ArrayList<>();
                    parseIndentation(indentation.indentationSetup, definitionArg);
                }
            }
        }
    }

    private void parseIndentation(List<IndentationSetting> indentationSetup, LispContainer definitionArg) {
        int ix = 0;
        while (ix < definitionArg.getItems().size()) {
            LispElement e = definitionArg.getItems().get(ix++);
            ix = parseSingleElement(e, definitionArg, indentationSetup, ix);
        }
    }

    private int parseSingleElement(LispElement e, LispContainer definitionArg, List<IndentationSetting> indentationSetup, int ix) {
        if (e instanceof LispSymbol symbol) {
            if (symbol.getValue().equalsIgnoreCase("NIL")) {
                indentationSetup.add(null);
            } else if (symbol.getValue().equals("&lambda")) {
                IndentationSetting setting = new IndentationSetting();
                setting.type = IndentationType.LAMBDA;
                indentationSetup.add(setting);
            } else if (symbol.getValue().equals("&rest")) {
                LispElement e2 = definitionArg.getItems().get(ix++);
                List<IndentationSetting> innerSetting = new ArrayList<>();
                ix = parseSingleElement(e2, definitionArg, innerSetting, ix);
                IndentationSetting setting = new IndentationSetting();
                indentationSetup.add(setting);
                setting.type = IndentationType.REST;
                setting.inner = innerSetting.get(0);
            } else if (symbol.getValue().equals("&body")) {
                IndentationSetting setting = new IndentationSetting();
                indentationSetup.add(setting);
                setting.type = IndentationType.BODY;
            } else if (symbol.getValue().equals("&whole")) {
                LispElement e2 = definitionArg.getItems().get(ix++);
                List<IndentationSetting> innerSetting = new ArrayList<>();
                ix = parseSingleElement(e2, definitionArg, innerSetting, ix);
                IndentationSetting setting = new IndentationSetting();
                indentationSetup.add(setting);
                setting.type = IndentationType.WHOLE;
                setting.inner = innerSetting.get(0);
            }
        } else if (e instanceof LispInteger integer) {
            IndentationSetting setting = new IndentationSetting();
            setting.value = integer.getValue().intValue();
            indentationSetup.add(setting);
        } else if (e instanceof LispContainer container) {
            IndentationSetting setting = new IndentationSetting();
            setting.listSetting = new ArrayList<>();
            indentationSetup.add(setting);
            parseIndentation(setting.listSetting, container);
        }
        return ix;
    }

    public Integer calculateOffset(PsiElement element, PsiFile file, boolean wasAfter, String documentText, int offset) {
        SltIndentationSettings settings = SltIndentationSettings.getInstance();
        String packageName = LispParserUtil.getPackage(file, offset);
        if (wasAfter) {
            if (element.getParent() != file) {
                PsiElement parent = element.getParent();
                while (parent != null && !(parent instanceof LispToplevel)) {
                    parent = parent.getParent();
                }
                if (parent == null || parent.getLastChild() == element ||
                        PsiTreeUtil.lastChild(parent) == element) {
                    // it is fully parsed and thus valid form, ie just do general offset 0
                    return 0;
                }
            }
        }

        List<Integer> indentList = new ArrayList<>();
        if (element.getParent() != file) {
            // we are in correct form
            LispToplevel toplevel = PsiTreeUtil.getParentOfType(element, LispToplevel.class);
            assert toplevel != null;
            int numBraces = 0;

            LispList parent = PsiTreeUtil.getParentOfType(element, LispList.class);
            while (parent != null) {
                ++numBraces;
                int startIndent = getOffsetSinceNewLine(documentText, parent.getTextOffset());
                indentList.add(startIndent);
                parent = PsiTreeUtil.getParentOfType(parent, LispList.class);
            }

            try {
                String formText = documentText.substring(toplevel.getTextOffset(), offset);
                formText += StringUtils.repeat(')', numBraces);
                return calculateOffset(formText, file.getProject(), indentList, settings, packageName);
            } catch (Exception ignored) {

            }
        } else {
            int numBraces = element.getNode().getElementType() == LispTypes.RPAREN ? -1 : 0;
            PsiElement previousToplevel = element.getPrevSibling();
            while (!(previousToplevel instanceof LispToplevel)) {
                if (previousToplevel == null)
                    break;

                if (previousToplevel.getNode().getElementType() == LispTypes.LPAREN) {
                    numBraces++;
                    if (numBraces == 0) {
                        int startIndent = getOffsetSinceNewLine(documentText, previousToplevel.getTextOffset());
                        indentList.add(startIndent);
                    }
                }
                if (previousToplevel.getNode().getElementType() == LispTypes.RPAREN) {
                    numBraces--;
                }
                previousToplevel = previousToplevel.getPrevSibling();
            }
            int startOffset = 0;
            if (previousToplevel != null) {
                startOffset = previousToplevel.getNextSibling().getTextOffset();
            }

            try {
                String formText = documentText.substring(startOffset, offset);
                formText += StringUtils.repeat(')', numBraces);
                return calculateOffset(formText, file.getProject(), indentList, settings, packageName);
            } catch (Exception ignored) {

            }
        }
        return !indentList.isEmpty() ? indentList.get(0) : 0;
    }

    private int getOffsetSinceNewLine(String documentText, int textOffset) {
        int offset = 0;
        while (textOffset >= 0) {
            if (documentText.charAt(textOffset) == '\n') {
                break;
            }
            ++offset;
            --textOffset;
        }
        return offset;
    }

    private Integer calculateOffset(String formText, Project project, List<Integer> indentList, SltIndentationSettings settings, String packageName) throws Exception {
        PsiFileFactory factory = PsiFileFactory.getInstance(project);
        PsiFile source = factory.createFileFromText("fragment.cl", SltCommonLispFileType.INSTANCE, formText);
        List<LispElement> elements = LispUtils.convertAst(source);
        if (elements.isEmpty())
            return !indentList.isEmpty() ? indentList.get(0) : 0;
        LispElement element = elements.get(0);
        if (element instanceof LispContainer container) {
            return calculateOffsetForForm(container, indentList, project, settings, packageName);
        }
        return !indentList.isEmpty() ? indentList.get(0) : 0;
    }

    private Integer calculateOffsetForForm(LispContainer container, List<Integer> indentList, Project project,
                                           SltIndentationSettings settings, String packageName) throws Exception {
        if (container.getItems().isEmpty())
            return !indentList.isEmpty() ? indentList.get(0) : 0;

        LispElement head = container.getItems().get(0);
        LispElement lastItem = container.getItems().get(container.getItems().size() - 1);

        Integer calculatedSubOffset = null;
        if (lastItem instanceof LispContainer subcontainer) {
            if (head == lastItem)
                return !indentList.isEmpty() ? indentList.get(0) : 0;
            calculatedSubOffset = calculateOffsetForForm(subcontainer, indentList.subList(1, indentList.size()-1),
                    project, settings, packageName);
        }

        if (!(head instanceof LispSymbol headSymbol)) {
            return !indentList.isEmpty() ? indentList.get(0) : 0;
        }

        return settings.restIndentation + (!indentList.isEmpty() ? indentList.get(0) : 0);
    }

    private static class IndentationUpdate {

        private String name;
        private BigInteger bodyArg;
        private final Set<String> packages = new HashSet<>();

    }

    private static class ManualIndentation {

        public String ref;
        public Integer argCount;
        public List<IndentationSetting> indentationSetup;

        @Override
        public String toString() {
            if (ref != null) {
                return "(as " + ref + ")";
            } else if (argCount != null) {
                return "" + argCount;
            } else if (indentationSetup != null) {
                return "(" + indentationSetup.stream().map(x -> x == null ? "NIL" : x.toString())
                        .collect(Collectors.joining(" ")) + ")";
            }
            return "NIL";
        }

    }

    private static class IndentationSetting {

        public Integer value;
        public IndentationType type;
        public IndentationSetting inner;
        public List<IndentationSetting> listSetting;

        @Override
        public String toString() {
            if (value != null) {
                return "" + value;
            } else if (type != null) {
                return switch (type) {
                    case LAMBDA -> "&lambda";
                    case REST -> "&rest " + (inner == null ? "NIL" : inner.toString());
                    case BODY -> "&body";
                    case WHOLE -> "&whole " + (inner == null ? "NIL" : inner.toString());
                };
            } else if (listSetting != null){
                return "(" + listSetting.stream().map(x -> x == null ? "NIL" : x.toString())
                        .collect(Collectors.joining(" ")) + ")";
            }
            return "NIL";
        }
    }

    private enum IndentationType {
        LAMBDA, REST, BODY, WHOLE

    }

}

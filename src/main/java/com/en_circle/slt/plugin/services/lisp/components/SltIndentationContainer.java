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
                indentation.normalArgumentCount = ((LispInteger) definition.getItems().get(1)).getValue().intValue();
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

    public Integer calculateOffset(PsiElement element, PsiFile file, boolean wasAfter, String documentText, int offset,
                                   String packageOverride) {
        SltIndentationSettings settings = SltIndentationSettings.getInstance();
        String packageName = packageOverride != null ? packageOverride : LispParserUtil.getPackage(file, offset);
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

        if (element.getParent() != file) {
            // we are in correct form
            LispToplevel toplevel = PsiTreeUtil.getParentOfType(element, LispToplevel.class);
            assert toplevel != null;
            int numBraces = 0;

            LispList parent = PsiTreeUtil.getParentOfType(element, LispList.class);
            while (parent != null) {
                ++numBraces;
                parent = PsiTreeUtil.getParentOfType(parent, LispList.class);
            }

            try {
                String formText = documentText.substring(toplevel.getTextOffset(), offset);
                if (wasAfter) {
                    // insert fake element at the end, so we identify correct form
                    formText += " 0";
                }
                formText += StringUtils.repeat(')', numBraces);
                return calculateOffset(formText, file.getProject(), settings, packageName);
            } catch (Exception ignored) {

            }
        } else {
            int numLeftBraces = 0;
            int numRightBraces = element.getNode().getElementType() == LispTypes.RPAREN ? 1 : 0;
            PsiElement previousToplevel = element.getPrevSibling();
            while (!(previousToplevel instanceof LispToplevel)) {
                if (previousToplevel == null)
                    break;

                if (previousToplevel.getNode().getElementType() == LispTypes.LPAREN) {
                    if (numRightBraces > 0) {
                        numRightBraces--;
                    } else {
                        numLeftBraces++;
                    }
                }
                if (previousToplevel.getNode().getElementType() == LispTypes.RPAREN) {
                    numRightBraces--;
                }
                previousToplevel = previousToplevel.getPrevSibling();
            }
            int startOffset = 0;
            if (previousToplevel != null) {
                startOffset = previousToplevel.getNextSibling().getTextOffset();
            }

            try {
                String formText = documentText.substring(startOffset, offset);
                // insert fake element at the end, so we identify correct form
                formText += " 0" + StringUtils.repeat(')', numLeftBraces);
                return calculateOffset(formText, file.getProject(), settings, packageName);
            } catch (Exception ignored) {

            }
        }
        return 0;
    }

    private Integer calculateOffset(String formText, Project project, SltIndentationSettings settings, String packageName) throws Exception {
        Map<LispElement, Integer> formIndentation = new IdentityHashMap<>();
        PsiFileFactory factory = PsiFileFactory.getInstance(project);
        PsiFile source = factory.createFileFromText("fragment.cl", SltCommonLispFileType.INSTANCE, formText);
        List<LispElement> elements = LispUtils.convertAst(source, formIndentation, formText);
        if (elements.isEmpty())
            return 0;
        LispElement element = elements.get(0);
        if (element instanceof LispContainer container) {
            Stack<IndentationBackTrack> backTrackStack = new Stack<>();
            return calculateOffsetForForm(container, formIndentation, project, settings, packageName, backTrackStack);
        }
        return formIndentation.getOrDefault(element, 0);
    }

    private Integer calculateOffsetForForm(LispContainer container, Map<LispElement, Integer> formIndentation, Project project,
                                           SltIndentationSettings settings, String packageName,
                                           Stack<IndentationBackTrack> backTrackStack) throws Exception {
        if (container.getItems().isEmpty())
            return formIndentation.getOrDefault(container, 0);

        LispElement head = container.getItems().get(0);
        LispElement lastItem = container.getItems().get(container.getItems().size() - 1);
        int argPos = container.getItems().size() - 1;
        int argPosWithoutHead = container.getItems().size() - 2;

        if (head == lastItem)
            return formIndentation.getOrDefault(head, 0);

        Integer calculatedSubOffset = null;
        if (lastItem instanceof LispContainer subcontainer) {
            backTrackStack.add(new IndentationBackTrack(container, container.getItems().size() - 1));
            calculatedSubOffset = calculateOffsetForForm(subcontainer, formIndentation,
                    project, settings, packageName, backTrackStack);
            backTrackStack.pop();
            return calculatedSubOffset;
        }

        if (!(head instanceof LispSymbol headSymbol)) {
            return formIndentation.getOrDefault(lastItem, 0);
        }

        int additionalOffset = 0;
        IndentationSetting appliedSetting = null;

        ManualIndentation indentation = getManualIndentation(((LispSymbol) head).getValue());
        List<IndentationSetting> listOfSettings = null;
        if (indentation == null) {
            indentation = getMacroIndentation(((LispSymbol) head).getValue(), packageName);
        }
        if (indentation == null) {
            BacktrackInformation backtrackIndentation = getBacktrackIndentation(backTrackStack);
            if (backtrackIndentation != null) {
                indentation = backtrackIndentation.indentation;
                appliedSetting = backtrackIndentation.setting;
                additionalOffset = backtrackIndentation.additionalIndentation;
                listOfSettings = backtrackIndentation.listOfSettings;
            }
        }

        int appliedOffset = settings.restIndentation;
        assert argPosWithoutHead >= 0;

        if (indentation != null) {
            if (appliedSetting == null) {
                // we are not backtracking and/or this is simple apply
                if (indentation.normalArgumentCount != null) {
                    // Applying: * an integer N, meaning indent the first N arguments like
                    //                  function arguments, and any further arguments like a body.
                    //                  This is equivalent to (4 4 ... &body).
                    if (argPosWithoutHead < indentation.normalArgumentCount) {
                        appliedOffset = additionalOffset + settings.parameterIndentation;
                    } else {
                        appliedOffset = additionalOffset + settings.bodyIndentation;
                    }
                } else if (indentation.indentationSetup != null) {
                    listOfSettings = indentation.indentationSetup;

                    if (argPosWithoutHead < indentation.indentationSetup.size()) {
                        appliedSetting = indentation.indentationSetup.get(argPosWithoutHead);
                    } else {
                        IndentationSetting last = indentation.indentationSetup.get(indentation.indentationSetup.size() - 1);
                        if (last.type != null) {
                            if (last.type == IndentationType.REST || last.type == IndentationType.BODY) {
                                appliedSetting = last;
                            }
                        }
                    }
                }
            }

            if (appliedSetting != null) {
                if (appliedSetting.type != null) {
                    if (appliedSetting.type == IndentationType.REST) {
                        // * &rest.  When used, this must be the penultimate element.  The
                        //    element after this one applies to all remaining arguments.

                        additionalOffset += findAdditionalOffset(listOfSettings, appliedSetting, settings);
                        int restOffset = settings.restIndentation;
                        if (appliedSetting.value != null) {
                            restOffset = appliedSetting.value;
                        }
                        appliedOffset = additionalOffset + restOffset;
                    } else if (appliedSetting.type == IndentationType.BODY) {
                        // * &body.  This is equivalent to &rest lisp-body-indent, i.e., indent
                        //    all remaining elements by `lisp-body-indent'.

                        additionalOffset += findAdditionalOffset(listOfSettings, appliedSetting, settings);
                        appliedOffset = additionalOffset + settings.bodyIndentation;
                    } else if (appliedSetting.type == IndentationType.LAMBDA) {
                        // * &lambda.  Indent the argument (which may be a list) by 4.

                        additionalOffset += findAdditionalOffset(listOfSettings, appliedSetting, settings);
                        appliedOffset = additionalOffset + settings.lambdaIndentation;
                    } else if (appliedSetting.type == IndentationType.WHOLE) {

                        appliedOffset = additionalOffset +
                                (appliedSetting.value == null ? settings.defaultIndentation : appliedSetting.value);
                    }
                }
            }
        }

        return appliedOffset + formIndentation.getOrDefault(container, 0);
    }

    private int findAdditionalOffset(List<IndentationSetting> listOfSettings, IndentationSetting appliedSetting,
                                     SltIndentationSettings settings) {
        int ix = listOfSettings.indexOf(appliedSetting);
        for (int i=ix-1; ix>=0; ix--) {
            IndentationSetting setting = listOfSettings.get(ix);
            if (setting != null) {
                if (setting.type == IndentationType.WHOLE) {
                    return setting.value == null ? settings.defaultIndentation : setting.value;
                }
            }
        }
        return 0;
    }

    private ManualIndentation getMacroIndentation(String value, String packageName) {
        IndentationUpdate update = swankReportedIndentations.get(value);
        if (update != null) {
            if (update.packages.contains(packageName.toUpperCase())) {
                ManualIndentation indentation = new ManualIndentation();
                if (update.bodyArg != null) {
                    indentation.normalArgumentCount = update.bodyArg.intValue();
                }
                return indentation;
            }
        }
        return null;
    }

    private ManualIndentation getManualIndentation(String value) {
        value = value.toUpperCase();
        ManualIndentation indentation = defaultIndentations.get(value);
        while (indentation != null && indentation.ref != null) {
            indentation = defaultIndentations.get(indentation.ref);
        }
        return indentation;
    }

    private BacktrackInformation getBacktrackIndentation(Stack<IndentationBackTrack> backTrackStack) {
        if (backTrackStack.isEmpty()) {
            return null;
        }
        // TODO: backtrack
        return null;
    }

    private static class IndentationUpdate {

        private String name;
        private BigInteger bodyArg;
        private final Set<String> packages = new HashSet<>();

    }

    private static class ManualIndentation {

        public String ref;
        public Integer normalArgumentCount;
        public List<IndentationSetting> indentationSetup;

        @Override
        public String toString() {
            if (ref != null) {
                return "(as " + ref + ")";
            } else if (normalArgumentCount != null) {
                return "" + normalArgumentCount;
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

    private static class IndentationBackTrack {

        private LispContainer parentContainer;
        private int position;

        public IndentationBackTrack(LispContainer parentContainer, int position) {
            this.parentContainer = parentContainer;
            this.position = position;
        }

    }

    private static class BacktrackInformation {
        int additionalIndentation = 0;
        ManualIndentation indentation;
        IndentationSetting setting;
        List<IndentationSetting> listOfSettings;
    }

}

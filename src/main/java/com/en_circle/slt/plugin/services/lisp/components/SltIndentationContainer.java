package com.en_circle.slt.plugin.services.lisp.components;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.indentation.SltIndentationSettings;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils.OffsetInfo;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.en_circle.slt.templates.Indentation;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class SltIndentationContainer {

    private final Map<String, IndentationUpdate> swankReportedIndentations = new HashMap<>();
    private final Map<String, ManualIndentation> defaultIndentations = new HashMap<>();

    public void init(Project project) {
        String data = new Indentation().render();
        PsiFileFactory factory = PsiFileFactory.getInstance(project);
        PsiFile source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        loadBaseIndentation(LispUtils.convertAst(source));
    }

    public synchronized void update(LispContainer updates) {
        for (LispElement element : updates.getItems()) {
            LispContainer macro = (LispContainer) element;
            LispString symbolNameElement = (LispString) macro.getItems().get(0);
            String symbolName = symbolNameElement.getValue().toUpperCase();
            IndentationUpdate update = swankReportedIndentations.computeIfAbsent(symbolName, s -> new IndentationUpdate());
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
            } else if (definition.getItems().get(1) instanceof LispSymbol symbol) {
                String refMethod = symbol.getValue();
                try {
                    Method method = SltIndentationContainer.class.getDeclaredMethod(refMethod, LispElement.class,
                            LispContainer.class, Map.class, Project.class, SltIndentationSettings.class,
                            String.class, Stack.class);
                    method.setAccessible(true);
                    indentation.indentationImplementation = (topLevel, container, formIndentation, project1, settings, packageName, backTrackStack) ->
                            (int) method.invoke(this, topLevel, container, formIndentation, project1, settings, packageName, backTrackStack);
                } catch (Throwable ignored) {

                }
            } else {
                LispContainer definitionArg = (LispContainer) definition.getItems().get(1);
                if (definitionArg.getItems().get(0) instanceof LispSymbol && ((LispSymbol) definitionArg.getItems().get(0)).getValue().equalsIgnoreCase("as")) {
                    indentation.ref = ((LispSymbol) definitionArg.getItems().get(1)).getValue().toUpperCase();
                } else {
                    indentation.indentationSetup = new ArrayList<>();
                    Stack<Integer> whole = new Stack<>();
                    parseIndentation(indentation.indentationSetup, definitionArg, whole, x -> indentation.whole = x);
                }
            }
        }
    }

    private void parseIndentation(List<IndentationSetting> indentationSetup, LispContainer definitionArg, Stack<Integer> whole,
                                  Consumer<Integer> applyParentWhole) {
        int ix = 0;
        whole.push(0);
        while (ix < definitionArg.getItems().size()) {
            LispElement e = definitionArg.getItems().get(ix++);
            ix = parseSingleElement(e, definitionArg, indentationSetup, ix, whole, applyParentWhole);
        }
        whole.pop();
    }

    private int parseSingleElement(LispElement e, LispContainer definitionArg, List<IndentationSetting> indentationSetup, int ix,
            Stack<Integer> whole, Consumer<Integer> applyParentWhole) {
        if (e instanceof LispSymbol symbol) {
            if (symbol.getValue().equalsIgnoreCase("NIL")) {
                indentationSetup.add(new IndentationSetting());
            } else if (symbol.getValue().equals("&lambda")) {
                IndentationSetting setting = new IndentationSetting();
                setting.whole = whole.peek();
                setting.type = IndentationType.LAMBDA;
                indentationSetup.add(setting);
            } else if (symbol.getValue().equals("&rest")) {
                LispElement e2 = definitionArg.getItems().get(ix++);
                List<IndentationSetting> innerSetting = new ArrayList<>();
                ix = parseSingleElement(e2, definitionArg, innerSetting, ix, whole, applyParentWhole);
                IndentationSetting setting = new IndentationSetting();
                setting.whole = whole.peek();
                indentationSetup.add(setting);
                setting.type = IndentationType.REST;
                setting.inner = innerSetting.get(0);
            } else if (symbol.getValue().equals("&body")) {
                IndentationSetting setting = new IndentationSetting();
                setting.whole = whole.peek();
                indentationSetup.add(setting);
                setting.type = IndentationType.BODY;
            } else if (symbol.getValue().equals("&whole")) {
                LispElement e2 = definitionArg.getItems().get(ix++);
                int wholeAdd = ((LispInteger) e2).getValue().intValue();
                int currentWhole = whole.pop() + (Integer) whole.stream().mapToInt(Integer::intValue).sum();
                whole.push(currentWhole + wholeAdd);
                if (ix == 2) {
                    applyParentWhole.accept(currentWhole + wholeAdd);
                }
            } else {
                String refMethod = symbol.getValue();
                try {
                    Method method = SltIndentationContainer.class.getDeclaredMethod(refMethod, LispElement.class,
                            LispContainer.class, Map.class, Project.class, SltIndentationSettings.class,
                            String.class, Stack.class);
                    method.setAccessible(true);
                    IndentationSetting setting = new IndentationSetting();
                    setting.indentationImplementation = (topLevel, container, formIndentation, project1, settings, packageName, backTrackStack) ->
                            (int) method.invoke(this, topLevel, container, formIndentation, project1, settings, packageName, backTrackStack);
                    indentationSetup.add(setting);
                } catch (Throwable ignored) {

                }
            }
        } else if (e instanceof LispInteger integer) {
            IndentationSetting setting = new IndentationSetting();
            setting.whole = whole.peek();
            setting.value = integer.getValue().intValue();
            indentationSetup.add(setting);
        } else if (e instanceof LispContainer container) {
            IndentationSetting setting = new IndentationSetting();
            setting.whole = whole.peek();
            setting.listSetting = new ArrayList<>();
            indentationSetup.add(setting);
            parseIndentation(setting.listSetting, container, whole, w -> setting.listAdd = w);
        }
        return ix;
    }

    public Integer calculateOffset(PsiElement element, PsiFile file, boolean wasAfter, String documentText, int offset,
                                   String packageOverride) {
        SltIndentationSettings settings = SltIndentationSettings.getInstance(element.getProject());
        String packageName = packageOverride != null ? packageOverride : LispParserUtil.getPackage(file, offset);
        if (wasAfter) {
            PsiManager manager = PsiManager.getInstance(file.getProject());
            if (!manager.areElementsEquivalent(element.getParent(), file)) {
                PsiElement parent = element.getParent();
                while (parent != null && !(parent instanceof LispToplevel)) {
                    parent = parent.getParent();
                }
                if (parent == null || manager.areElementsEquivalent(parent.getLastChild(), element) ||
                        manager.areElementsEquivalent(PsiTreeUtil.lastChild(parent), element)) {
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
            int numLeftBraces = element.getNode().getElementType() == LispTypes.LPAREN ? 1 : 0;
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
        Map<LispElement, OffsetInfo> formIndentation = new IdentityHashMap<>();
        PsiFileFactory factory = PsiFileFactory.getInstance(project);
        PsiFile source = factory.createFileFromText("fragment.cl", SltCommonLispFileType.INSTANCE, formText);
        List<LispElement> elements = LispUtils.convertAst(source, formIndentation, formText);
        if (elements.isEmpty())
            return 0;
        LispElement element = elements.get(0);
        if (element instanceof LispContainer container) {
            Stack<IndentationBackTrack> backTrackStack = new Stack<>();
            return calculateOffsetForForm(element, container, formIndentation, project, settings, packageName, backTrackStack);
        }
        return formIndentation.getOrDefault(element, OffsetInfo.DEFAULT).base;
    }

    private Integer calculateOffsetForForm(LispElement topLevel, LispContainer container,
                                           Map<LispElement, OffsetInfo> formIndentation, Project project,
                                           SltIndentationSettings settings, String packageName,
                                           Stack<IndentationBackTrack> backTrackStack) throws Exception {
        if (container.getItems().isEmpty())
            return formIndentation.getOrDefault(container, OffsetInfo.DEFAULT).parentForm;

        LispElement head = container.getItems().get(0);
        LispElement lastItem = container.getItems().get(container.getItems().size() - 1);
        int argPos = container.getItems().size() - 1;
        int argPosWithoutHead = container.getItems().size() - 2;
        boolean inSubform = backTrackStack.size() != 0;
        boolean hasHead;

        if (head == lastItem && !inSubform)
            return formIndentation.getOrDefault(head, OffsetInfo.DEFAULT).parentForm;
        hasHead = head != lastItem;

        Integer calculatedSubOffset;
        if (lastItem instanceof LispContainer subcontainer) {
            backTrackStack.add(new IndentationBackTrack(container, container.getItems().size() - 1));
            calculatedSubOffset = calculateOffsetForForm(topLevel, subcontainer, formIndentation,
                    project, settings, packageName, backTrackStack);
            backTrackStack.pop();
            return calculatedSubOffset;
        }

        if (!(head instanceof LispSymbol) && !inSubform) {
            return formIndentation.getOrDefault(topLevel, OffsetInfo.DEFAULT).base;
        }
        if (!(head instanceof LispSymbol)) {
            hasHead = false;
        }

        int additionalOffset = 0;
        IndentationSetting appliedSetting = null;
        ManualIndentation indentation = null;
        List<IndentationSetting> listOfSettings = null;
        IndentationImplementation indentationImplementation = null;
        boolean returnedFromBacktrack = false;

        if (!hasHead) {
            backTrackStack.add(new IndentationBackTrack(container, container.getItems().size() - 1));
            BacktrackInformation backtrackIndentation = getBacktrackIndentation(packageName, backTrackStack, settings);
            backTrackStack.pop();
            if (backtrackIndentation != null) {
                appliedSetting = backtrackIndentation.setting;
                listOfSettings = backtrackIndentation.listOfSettings;
                returnedFromBacktrack = true;
            }
        } else {
            indentation = getManualIndentation(((LispSymbol) head).getValue());

            if (indentation == null) {
                indentation = getMacroIndentation(((LispSymbol) head).getValue(), packageName);
            }
            if (indentation == null) {
                backTrackStack.add(new IndentationBackTrack(container, container.getItems().size() - 1));
                BacktrackInformation backtrackIndentation = getBacktrackIndentation(packageName, backTrackStack, settings);
                backTrackStack.pop();
                if (backtrackIndentation != null) {
                    appliedSetting = backtrackIndentation.setting;
                    listOfSettings = backtrackIndentation.listOfSettings;
                    additionalOffset = backtrackIndentation.parentWhole;
                    returnedFromBacktrack = true;
                }
            }
        }

        int appliedOffset = settings.restIndentation;
        if (!hasHead || returnedFromBacktrack) {
            argPosWithoutHead = argPos;
        }
        assert argPosWithoutHead >= 0;

        if (indentation != null) {
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
                    appliedSetting = indentation.indentationSetup.get(indentation.indentationSetup.size() - 1);
                }
            } else if (indentation.indentationImplementation != null) {
                // custom callback
                indentationImplementation = indentation.indentationImplementation;
            }
        }

        if (returnedFromBacktrack) {
            if (argPosWithoutHead < listOfSettings.size()) {
                appliedSetting = listOfSettings.get(argPosWithoutHead);
            } else {
                appliedSetting = listOfSettings.get(listOfSettings.size() - 1);
            }
        }

        if (appliedSetting != null) {
            IndentationSetting originalSetting = appliedSetting;
            if (appliedSetting.type == IndentationType.REST) {
                // * &rest.  When used, this must be the penultimate element.  The
                //    element after this one applies to all remaining arguments.
                appliedSetting = appliedSetting.inner;
            }
            if (appliedSetting != null) {
                if (appliedSetting.type != null) {
                    if (appliedSetting.type == IndentationType.REST) {
                        // should not happen!
                    } else if (appliedSetting.type == IndentationType.BODY) {
                        // * &body.  This is equivalent to &rest lisp-body-indent, i.e., indent
                        //    all remaining elements by `lisp-body-indent'.
                        additionalOffset += findAdditionalOffset(originalSetting);
                        appliedOffset = additionalOffset + settings.bodyIndentation;
                    } else if (appliedSetting.type == IndentationType.LAMBDA) {
                        // * &lambda.  Indent the argument (which may be a list) by 4.
                        additionalOffset += findAdditionalOffset(originalSetting);
                        appliedOffset = additionalOffset + settings.lambdaIndentation;
                    }
                } else {
                    if (appliedSetting.value != null) {
                        additionalOffset += findAdditionalOffset(originalSetting);
                        appliedOffset = additionalOffset + appliedSetting.value;
                    } else if (appliedSetting.listSetting != null) {
                        appliedOffset = additionalOffset + appliedSetting.listAdd;
                    }
                }
            }

            if (appliedSetting != null && appliedSetting.indentationImplementation != null) {
                additionalOffset += findAdditionalOffset(originalSetting);
                indentationImplementation = appliedSetting.indentationImplementation;
            }
        } else {
            if (indentationImplementation == null) {
                // special case, we failed to match, thus get this form's start offset + base offset
                return appliedOffset + formIndentation.getOrDefault(lastItem, OffsetInfo.DEFAULT).parentForm;
            }
        }

        if (indentationImplementation != null) {
            return additionalOffset + indentationImplementation.calculateOffsetForForm(topLevel, container, formIndentation,
                    project, settings, packageName, backTrackStack);
        }

        return appliedOffset + formIndentation.getOrDefault(topLevel, OffsetInfo.DEFAULT).base;
    }

    private int findAdditionalOffset(IndentationSetting appliedSetting) {
        if (appliedSetting != null)
            return appliedSetting.whole;
        return 0;
    }

    private ManualIndentation getMacroIndentation(String value, String packageName) {
        IndentationUpdate update = swankReportedIndentations.get(value.toUpperCase());
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

    private BacktrackInformation getBacktrackIndentation(String packageName,
                                                         Stack<IndentationBackTrack> backTrackStack,
                                                         SltIndentationSettings sltSettings) {
        if (backTrackStack.isEmpty()) {
            return null;
        }

        int ix = backTrackStack.size();
        return doGetBacktrackIndentation(packageName, backTrackStack, ix-1, sltSettings);
    }

    private BacktrackInformation doGetBacktrackIndentation(String packageName, Stack<IndentationBackTrack> backTrackStack,
                                                           int i, SltIndentationSettings sltSettings) {
        if (i < 0) {
            return null;
        }
        IndentationBackTrack backTrack = backTrackStack.get(i);

        LispElement head = backTrack.parentContainer.getItems().get(0);

        if (!(head instanceof LispSymbol)) {
            return doGetBacktrackIndentation(packageName, backTrackStack, i-1, sltSettings);
        }

        ManualIndentation indentation = getManualIndentation(((LispSymbol) head).getValue());

        if (indentation == null) {
            indentation = getMacroIndentation(((LispSymbol) head).getValue(), packageName);
        }
        if (indentation != null) {
            // validate they all must be nested list except for last and they all must be in settings
            // * a list, with elements as described above.  This applies when the
            //    associated function argument is itself a list.  Each element of the list
            //    specifies how to indent the associated argument.
            if (indentation.indentationSetup != null) {
                boolean valid = false;
                BacktrackInformation information = new BacktrackInformation();

                int clevel = i;
                int parentWhole = 0;
                boolean first = true;

                List<IndentationSetting> settings = indentation.indentationSetup;
                while (settings != null) {
                    if (backTrackStack.size() <= clevel) {
                        break;
                    }

                    int pos = backTrackStack.get(clevel).position;
                    if (first) {
                        pos = pos - 1;
                    }
                    LispElement element = backTrackStack.get(clevel).parentContainer.getItems().get(first ?  1 + pos : pos);
                    IndentationSetting setting;
                    if (pos >= settings.size()) {
                        setting = settings.get(settings.size() - 1);
                    } else {
                        setting = settings.get(pos);
                    }
                    if (setting.type == IndentationType.REST) {
                        setting = setting.inner;
                    }

                    if (clevel == backTrackStack.size() - 1) {
                        information.setting = setting;
                        information.listOfSettings = settings;
                        information.parentWhole = parentWhole;
                        valid = true;
                        break;
                    } else {
                        if (!(element instanceof LispContainer)) {
                            break;
                        }
                        if (setting.listSetting == null) {
                            settings = null;
                        } else {
                            settings = setting.listSetting;
                        }
                    }
                    parentWhole = setting.whole;
                    ++clevel;
                    first = false;
                }

                if (valid) {
                    return information;
                }
            }
            return null;
        }

        return doGetBacktrackIndentation(packageName, backTrackStack, i-1, sltSettings);
    }

    private static class IndentationUpdate {
        private BigInteger bodyArg;
        private final Set<String> packages = new HashSet<>();

    }

    private static class ManualIndentation {

        public String ref;
        public Integer normalArgumentCount;
        public List<IndentationSetting> indentationSetup;
        public IndentationImplementation indentationImplementation;
        public int whole = 0; // should never be set

        @Override
        public String toString() {
            if (ref != null) {
                return "(as " + ref + ")";
            } else if (normalArgumentCount != null) {
                return "" + normalArgumentCount;
            } else if (indentationSetup != null) {
                return "(" + indentationSetup.stream().map(x -> x == null ? "NIL" : x.toString())
                        .collect(Collectors.joining(" ")) + ")";
            } else if (indentationImplementation != null) {
                return "(" + indentationImplementation + ")";
            }
            return "NIL";
        }

    }

    private static class IndentationSetting {

        public Integer value;
        public IndentationType type;
        public IndentationSetting inner;
        public List<IndentationSetting> listSetting;
        public int listAdd; // listSetting != null -> apply this
        public IndentationImplementation indentationImplementation;
        public int whole = 0;

        @Override
        public String toString() {
            if (value != null) {
                return "" + value;
            } else if (type != null) {
                return switch (type) {
                    case LAMBDA -> "&lambda";
                    case REST -> "&rest " + (inner == null ? "NIL" : inner.toString());
                    case BODY -> "&body";
                };
            } else if (listSetting != null){
                return "(" + listSetting.stream().map(x -> x == null ? "NIL" : x.toString())
                        .collect(Collectors.joining(" ")) + ")";
            } else if (indentationImplementation != null) {
                return "(" + indentationImplementation + ")";
            }
            return "NIL";
        }
    }

    private enum IndentationType {
        LAMBDA, REST, BODY

    }

    private static class IndentationBackTrack {

        private final LispContainer parentContainer;
        private final int position;

        public IndentationBackTrack(LispContainer parentContainer, int position) {
            this.parentContainer = parentContainer;
            this.position = position;
        }

    }

    private static class BacktrackInformation {
        IndentationSetting setting;
        List<IndentationSetting> listOfSettings;
        int parentWhole;
    }

    public interface IndentationImplementation {

        Integer calculateOffsetForForm(LispElement topLevel, LispContainer container, Map<LispElement, OffsetInfo> formIndentation, Project project,
                                       SltIndentationSettings settings, String packageName,
                                       Stack<IndentationBackTrack> backTrackStack) throws Exception;

    }

    // specific implementations here:

    @SuppressWarnings("unused")
    Integer lispIndentLoop(LispElement topLevel, LispContainer container, Map<LispElement, OffsetInfo> formIndentation, Project project,
                           SltIndentationSettings settings, String packageName,
                           Stack<IndentationBackTrack> backTrackStack) throws Exception {
        return settings.bodyIndentation;
    }

}

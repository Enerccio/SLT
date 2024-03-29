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
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.apache.commons.lang3.StringUtils;

import java.math.BigInteger;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class SltIndentationContainer {

    private static final Set<String> loopBodyForms = new HashSet<>();
    static {
        for (String symbol : Arrays.asList(
                "do", "doing", "return", "collect", "collecting", "append", "appending",
                "nconc", "nconcing", "count", "counting", "sum", "summing", "maximize",
                "maximizing", "minimize", "minimizing"
                )) {
            loopBodyForms.add(symbol.toUpperCase());
            loopBodyForms.add(":" + symbol.toUpperCase());
        }
    }

    private final Map<String, IndentationImplementation> boundImplementations = new HashMap<>();
    private final Map<String, IndentationUpdate> swankReportedIndentations = new HashMap<>();
    private final Map<String, ManualIndentation> defaultIndentations = new HashMap<>();
    private final Map<String, ManualIndentation> specialIndentations = new HashMap<>();

    public SltIndentationContainer() {
        boundImplementations.put("lispIndentLoop".toUpperCase(), this::lispIndentLoop);
        boundImplementations.put("lispIndentDefsetf".toUpperCase(), this::lispIndentDefsetf);
        boundImplementations.put("lispIndentDefmethod".toUpperCase(), this::lispIndentDefmethod);
        boundImplementations.put("lispIndentTagbody".toUpperCase(), this::lispIndentTagbody);
    }

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
            if (formName.startsWith("&"))
                specialIndentations.put(formName, indentation);
            else
                defaultIndentations.put(formName, indentation);

            if (definition.getItems().get(1) instanceof LispInteger) {
                indentation.normalArgumentCount = ((LispInteger) definition.getItems().get(1)).getValue().intValue();
            } else if (definition.getItems().get(1) instanceof LispSymbol symbol) {
                String refMethod = symbol.getValue();
                if (boundImplementations.containsKey(refMethod.toUpperCase())) {
                    indentation.indentationImplementation = boundImplementations.get(refMethod.toUpperCase());
                }
            } else {
                LispContainer definitionArg = (LispContainer) definition.getItems().get(1);
                if (definitionArg.getItems().get(0) instanceof LispSymbol && ((LispSymbol) definitionArg.getItems().get(0)).getValue().equalsIgnoreCase("as")) {
                    indentation.ref = ((LispSymbol) definitionArg.getItems().get(1)).getValue().toUpperCase();
                } else {
                    indentation.indentationSetup = new ArrayList<>();
                    parseIndentation(indentation.indentationSetup, definitionArg, x -> indentation.whole = x);
                }
            }
        }
    }

    private void parseIndentation(List<IndentationSetting> indentationSetup, LispContainer definitionArg,
                                  Consumer<Integer> applyParentWhole) {
        int ix = 0;
        while (ix < definitionArg.getItems().size()) {
            LispElement e = definitionArg.getItems().get(ix++);
            ix = parseSingleElement(e, definitionArg, indentationSetup, ix, applyParentWhole);
        }
    }

    private int parseSingleElement(LispElement e, LispContainer definitionArg, List<IndentationSetting> indentationSetup, int ix,
            Consumer<Integer> applyParentWhole) {
        if (e instanceof LispSymbol symbol) {
            if (symbol.getValue().equalsIgnoreCase("NIL")) {
                indentationSetup.add(new IndentationSetting());
            } else if (symbol.getValue().equals("&lambda")) {
                IndentationSetting setting = new IndentationSetting();
                setting.type = IndentationType.LAMBDA;
                indentationSetup.add(setting);
            } else if (symbol.getValue().equals("&rest")) {
                LispElement e2 = definitionArg.getItems().get(ix++);
                List<IndentationSetting> innerSetting = new ArrayList<>();
                ix = parseSingleElement(e2, definitionArg, innerSetting, ix, applyParentWhole);
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
                int wholeAdd = ((LispInteger) e2).getValue().intValue();
                if (ix == 2) {
                    applyParentWhole.accept(wholeAdd);
                }
            } else {
                String refMethod = symbol.getValue();
                if (boundImplementations.containsKey(refMethod.toUpperCase())) {
                    IndentationSetting setting = new IndentationSetting();
                    setting.indentationImplementation = boundImplementations.get(refMethod.toUpperCase());
                    indentationSetup.add(setting);
                }
            }
        } else if (e instanceof LispInteger integer) {
            IndentationSetting setting = new IndentationSetting();
            setting.value = integer.getValue().intValue();
            indentationSetup.add(setting);
        } else if (e instanceof LispContainer container) {
            IndentationSetting setting = new IndentationSetting();
            setting.listSetting = new ArrayList<>();
            indentationSetup.add(setting);
            parseIndentation(setting.listSetting, container, w -> setting.listAdd = w);
        }
        return ix;
    }

    public Integer calculateIndent(PsiElement element, PsiFile file, boolean wasAfter, String documentText, int offset,
                                   String packageOverride) {
        IndentationState state = new IndentationState();

        SltIndentationSettings settings = SltIndentationSettings.getInstance(element.getProject());
        state.packageName = packageOverride != null ? packageOverride : LispParserUtil.getPackage(file, offset);
        state.settings = settings;

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
            if (toplevel == null) {
                // something very wrong, like bad lisp code, do not indent
                return 0;
            }

            int numBraces = 0;

            LispList parent = PsiTreeUtil.getParentOfType(element, LispList.class);
            while (parent != null) {
                ++numBraces;
                parent = PsiTreeUtil.getParentOfType(parent, LispList.class);
            }

            try {
                String formText = documentText.substring(toplevel.getTextOffset(),
                        element.getTextOffset() + element.getTextLength() - (wasAfter ? 0 : 1));
                LispList list = PsiTreeUtil.getParentOfType(element, LispList.class);
                if (list != null) {
                    if (element.getNode().getElementType() == LispTypes.RPAREN) {
                        list = PsiTreeUtil.getParentOfType(list, LispList.class);
                    }
                    if (list != null) {
                        if (list.getNextSibling() instanceof PsiErrorElement) {
                            if (element.getNode().getElementType() == LispTypes.RPAREN) {
                                if (numBraces > 0) {
                                    --numBraces;
                                }
                            }
                        }
                    }
                }
                state.hasRealElement = false;
                formText += " 0";
                formText += StringUtils.repeat(')', numBraces);
                return calculateIndent(state, formText, file.getProject());
            } catch (Exception ignored) {

            }
        } else {
            int numLeftBraces = element.getNode().getElementType() == LispTypes.LPAREN ? 1 : 0;
            int numRightBraces = 0;
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
                state.hasRealElement = false;
                formText += " 0" + StringUtils.repeat(')', numLeftBraces);
                return calculateIndent(state, formText, file.getProject());
            } catch (Exception ignored) {

            }
        }
        return 0;
    }

    private Integer calculateIndent(IndentationState state, String formText, Project project) throws Exception {
        state.project = project;
        state.formIndentation = new IdentityHashMap<>();
        PsiFileFactory factory = PsiFileFactory.getInstance(project);
        PsiFile source = factory.createFileFromText("fragment.cl", SltCommonLispFileType.INSTANCE, formText);
        List<LispElement> elements = LispUtils.convertAst(source, state.formIndentation, formText);
        if (elements.isEmpty())
            return 0;
        LispElement element = elements.get(0);
        if (element instanceof LispContainer container) {
            Stack<IndentationBackTrack> backTrackStack = new Stack<>();
            return calculateIndentForForm(state, element, container, backTrackStack);
        }
        return state.formIndentation.getOrDefault(element, OffsetInfo.DEFAULT).base;
    }

    protected Integer calculateIndentForForm(IndentationState state, LispElement topLevel, LispContainer container,
                                           Stack<IndentationBackTrack> backTrackStack) throws Exception {
        // if we have empty list just do same offset of parent form
        if (container.getItems().isEmpty())
            return state.formIndentation.getOrDefault(container, OffsetInfo.DEFAULT).parentForm;

        LispElement head = container.getItems().get(0);
        LispElement lastItem = container.getItems().get(container.getItems().size() - 1);
        // rules are shifted in source, sigh, so we have to shift here, but only if we have matching head,
        // otherwise we don't...
        int argPos = container.getItems().size() - 1;
        int argPosWithoutHead = container.getItems().size() - 2;
        boolean inSubform = backTrackStack.size() != 0;
        boolean hasHead;

        // we have toplevel form and head is only item in list, there is nothing to format...
        if (head == lastItem && !inSubform)
            return state.formIndentation.getOrDefault(head, OffsetInfo.DEFAULT).parentForm;
        hasHead = head != lastItem;

        Integer calculatedSubOffset;
        if (lastItem instanceof LispContainer subcontainer) {
            // last item in sexpr is list, thus we need to go further in.
            // offset is always calculated for innermost last element of whole toplevel ast tree

            // add information to backtrack, so we can backtrack later to search bottom rules
            backTrackStack.add(new IndentationBackTrack(container, container.getItems().size() - 1));
            calculatedSubOffset = calculateIndentForForm(state, topLevel, subcontainer, backTrackStack);
            backTrackStack.pop();
            return calculatedSubOffset;
        }

        if (!(head instanceof LispSymbol) && !inSubform) {
            // first element is a number, and we are toplevel, just return default offset ...
            return state.formIndentation.getOrDefault(topLevel, OffsetInfo.DEFAULT).base;
        }

        // if head is not symbol, but we possibly can be in subform we have to mark it so
        if (!(head instanceof LispSymbol)) {
            hasHead = false;
        }

        // applied inner setting, if matched
        IndentationSetting appliedSetting = null;
        // root indentation setting, if we matched head
        ManualIndentation indentation = null;
        // list of rules to select if applicable
        List<IndentationSetting> listOfSettings = null;
        // Custom callback rule - if non-null we call this instead of other rules
        IndentationImplementation indentationImplementation = null;
        // Returns true if we found our rule on backtracking, not by head
        boolean returnedFromBacktrack = false;
        // This denotes that backtrack has failed to fully apply rule and was forced to stop early, thus we
        // need to apply basic indentation + this elements active indentation
        LispElement backtrackRuleOnlyAppliedTo = null;
        LispElement toplevelOffset = topLevel;

        if (!hasHead) {
            // if head is not a symbol we don't bother with rule matching and instead go for backtrack immediately

            backTrackStack.add(new IndentationBackTrack(container, container.getItems().size() - 1));
            BacktrackInformation backtrackIndentation = getBacktrackIndentation(state, state.packageName, backTrackStack);
            backTrackStack.pop();
            if (backtrackIndentation != null) {
                // we found rule on backtrack
                backtrackRuleOnlyAppliedTo = backtrackIndentation.appliesRuleToOnly;
                if (backtrackRuleOnlyAppliedTo == null) {
                    // backtrack rule actually got hit
                    // if backtrackRuleOnlyAppliedTo is not null we only got partial hit some level below,
                    // so we skip all rules and just use that base element's indent + new base indent as rule
                    appliedSetting = backtrackIndentation.setting;
                    listOfSettings = backtrackIndentation.listOfSettings;
                    toplevelOffset = backtrackIndentation.rulematchedForm;
                } else {
                    toplevelOffset = backtrackRuleOnlyAppliedTo;
                }
                indentationImplementation = backtrackIndentation.manualCallback;
                returnedFromBacktrack = backtrackIndentation.exactHit;
            }
        } else {
            // get a base rule from indentation.cl
            indentation = getManualIndentation(state, ((LispSymbol) head).getValue());
            if (indentation != null) {
                toplevelOffset = container;
            }

            if (indentation == null) {
                // no base rule, try macro rule that we get from swank
                indentation = getMacroIndentation(((LispSymbol) head).getValue(), state.packageName);
                if (indentation != null) {
                    toplevelOffset = container;
                }
            }

            if (indentation == null) {
                // no macro or base rule, we have to backtrack
                backTrackStack.add(new IndentationBackTrack(container, container.getItems().size() - 1));
                BacktrackInformation backtrackIndentation = getBacktrackIndentation(state, state.packageName, backTrackStack);
                backTrackStack.pop();
                if (backtrackIndentation != null) {
                    // backtrack got hit
                    backtrackRuleOnlyAppliedTo = backtrackIndentation.appliesRuleToOnly;
                    if (backtrackRuleOnlyAppliedTo == null) {
                        // backtrack rule actually got hit
                        // if backtrackRuleOnlyAppliedTo is not null we only got partial hit some level below,
                        // so we skip all rules and just use that base element's indent + new base indent as rule
                        appliedSetting = backtrackIndentation.setting;
                        listOfSettings = backtrackIndentation.listOfSettings;
                        toplevelOffset = backtrackIndentation.rulematchedForm;
                    } else {
                        toplevelOffset = backtrackRuleOnlyAppliedTo;
                    }
                    indentationImplementation = backtrackIndentation.manualCallback;
                    returnedFromBacktrack = backtrackIndentation.exactHit;
                }
            }
        }

        if (indentationImplementation != null) {
            Integer offset = indentationImplementation.calculateOffsetForForm(state, topLevel, container, backTrackStack);
            if (offset != null) {
                return offset;
            }
        }

        int appliedOffset = state.settings.defaultIndentation;
        if (head == lastItem) {
            appliedOffset = state.settings.bodyIndentation;
        }

        if (backtrackRuleOnlyAppliedTo != null) {
            // partial matching, see above
            return appliedOffset + state.formIndentation.getOrDefault(backtrackRuleOnlyAppliedTo, OffsetInfo.DEFAULT).parentForm;
        }

        // if we found it on backtrack, head does not matter, so
        // we have to shift argument by 1 to normal position in rules
        if (!hasHead || returnedFromBacktrack) {
            argPosWithoutHead = argPos;
        }
        // just sanity check, so we don't query negative rule
        assert argPosWithoutHead >= 0;

        if (indentation != null) {
            // we are not backtracking and we found the main rule

            // since we actually matched head we consider this list "toplevel" in regard to indent
            topLevel = container;

            if (indentation.normalArgumentCount != null) {
                // Applying: * an integer N, meaning indent the first N arguments like
                //                  function arguments, and any further arguments like a body.
                //                  This is equivalent to (4 4 ... &body).
                if (argPosWithoutHead < indentation.normalArgumentCount) {
                    appliedOffset = state.settings.parameterIndentation;
                } else {
                    appliedOffset = state.settings.bodyIndentation;
                }
            } else if (indentation.indentationSetup != null) {
                // rule has multiple subrules, this handle it same as with backtrack, ie find correct subrule
                listOfSettings = indentation.indentationSetup;

                if (argPosWithoutHead < indentation.indentationSetup.size()) {
                    appliedSetting = indentation.indentationSetup.get(argPosWithoutHead);
                } else {
                    // any arguments above the rules are last rules (what is the point of &rest I don't get but
                    // slime indent documentation is dumb anyways
                    appliedSetting = indentation.indentationSetup.get(indentation.indentationSetup.size() - 1);
                }
            } else if (indentation.indentationImplementation != null) {
                // rule is callback to code, so we assign it here
                indentationImplementation = indentation.indentationImplementation;
            }
        }

        if (returnedFromBacktrack) {
            // backtrack always has subrules so same matching as above
            if (argPosWithoutHead < listOfSettings.size()) {
                appliedSetting = listOfSettings.get(argPosWithoutHead);
            } else {
                appliedSetting = listOfSettings.get(listOfSettings.size() - 1);
            }
        }

        if (appliedSetting != null) {
            // we actually have a matching rule

            if (appliedSetting.type == IndentationType.REST) {
                // * &rest.  When used, this must be the penultimate element.  The
                //    element after this one applies to all remaining arguments.
                appliedSetting = appliedSetting.inner; // overwrites itself
            }

            if (appliedSetting != null) {
                // another check because rest could have NULL subrule, and it would crash
                if (appliedSetting.type != null) {
                    // type matches if type is not null
                    if (appliedSetting.type == IndentationType.REST) {
                        // should not happen unless source rule is garbage (ie &rest &rest)
                        throw new IllegalStateException("bad rule setting");
                    } else if (appliedSetting.type == IndentationType.BODY) {
                        // * &body.  This is equivalent to &rest lisp-body-indent, i.e., indent
                        //    all remaining elements by `lisp-body-indent'.
                        appliedOffset = state.settings.bodyIndentation;
                    } else if (appliedSetting.type == IndentationType.LAMBDA) {
                        // * &lambda.  Indent the argument (which may be a list) by 4.
                        appliedOffset = state.settings.lambdaIndentation;
                    }
                } else {
                    // type is null so this rule either has value or is callback or is "list add"
                    if (appliedSetting.value != null) {
                        // just add value
                        appliedOffset = appliedSetting.value;
                    } else if (appliedSetting.listSetting != null) {
                        // this is "list add" rule, ie rule with listSetting not null but applied directly.
                        // list add rules are list rules that usually start with &whole.
                        // &whole argument is then applied both as list add for main list and as additional add
                        // for all inner rules (this is already calculated and baked into subrules with preserving hierarchy)
                        appliedOffset = appliedSetting.listAdd;
                    }
                }
            }

            if (appliedSetting != null && appliedSetting.indentationImplementation != null) {
                // we have callback so honor it
                indentationImplementation = appliedSetting.indentationImplementation;
            }
        } else {
            if (indentationImplementation == null) {
                // special case, we failed to match, thus get this form's start offset + base offset
                return appliedOffset + state.formIndentation.getOrDefault(toplevelOffset, OffsetInfo.DEFAULT).parentForm;
            }
        }

        if (indentationImplementation != null) {
            // call callback and let it deal with this shit
            Integer offset = indentationImplementation.calculateOffsetForForm(state, topLevel, container, backTrackStack);
            if (offset != null) {
                return offset;
            }
        }

        // general return of indent offset
        // can be just generic value or calculated value
        return appliedOffset + state.formIndentation.getOrDefault(container, OffsetInfo.DEFAULT).parentForm;
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

    private ManualIndentation getManualIndentation(IndentationState state, String value) {
        value = value.toUpperCase();

        if (value.equals(state.ruleOverrideSymbol)) {
            return state.ruleOverride;
        }

        ManualIndentation indentation = defaultIndentations.get(value);
        while (indentation != null && indentation.ref != null) {
            indentation = defaultIndentations.get(indentation.ref);
        }
        return indentation;
    }

    private BacktrackInformation getBacktrackIndentation(IndentationState state, String packageName,
                                                         Stack<IndentationBackTrack> backTrackStack) {
        if (backTrackStack.isEmpty()) {
            return null;
        }

        int ix = backTrackStack.size();
        return doGetBacktrackIndentation(state, packageName, backTrackStack, ix-1);
    }

    private BacktrackInformation doGetBacktrackIndentation(IndentationState state,
                                                           String packageName, Stack<IndentationBackTrack> backTrackStack,
                                                           int i) {
        if (i < 0) {
            // we reached the end of backtrack stack and found nothing, just bail
            return null;
        }
        IndentationBackTrack backTrack = backTrackStack.get(i);

        LispElement head = backTrack.parentContainer.getItems().get(0);

        if (!(head instanceof LispSymbol)) {
            return doGetBacktrackIndentation(state, packageName, backTrackStack, i-1);
        }

        ManualIndentation indentation = getManualIndentation(state, ((LispSymbol) head).getValue());

        if (indentation == null) {
            indentation = getMacroIndentation(((LispSymbol) head).getValue(), packageName);
        }
        if (indentation != null) {
            BacktrackInformation information = new BacktrackInformation();
            information.manualCallback = indentation.indentationImplementation;
            information.rulematchedForm = backTrack.parentContainer;
            // validate they all must be nested list except for last and they all must be in settings
            // * a list, with elements as described above.  This applies when the
            //    associated function argument is itself a list.  Each element of the list
            //    specifies how to indent the associated argument.
            if (indentation.indentationSetup != null) {
                int clevel = i;
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

                    information.appliesRuleToOnly = element;
                    if (clevel == backTrackStack.size() - 1) {
                        information.setting = setting;
                        information.listOfSettings = settings;
                        information.exactHit = true;
                        information.appliesRuleToOnly = null;
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
                    ++clevel;
                    first = false;
                }
            }
            return information;
        }

        return doGetBacktrackIndentation(state, packageName, backTrackStack, i-1);
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

    protected enum IndentationType {
        LAMBDA, REST, BODY

    }

    protected static class IndentationBackTrack {

        private final LispContainer parentContainer;
        private final int position;

        public IndentationBackTrack(LispContainer parentContainer, int position) {
            this.parentContainer = parentContainer;
            this.position = position;
        }

    }

    protected static class BacktrackInformation {
        boolean exactHit = false;
        IndentationSetting setting;
        List<IndentationSetting> listOfSettings;
        LispElement appliesRuleToOnly;
        LispElement rulematchedForm;
        IndentationImplementation manualCallback;
    }

    public static class IndentationState {
        public SltIndentationSettings settings;
        public String packageName;
        public Project project;
        public Map<LispElement, OffsetInfo> formIndentation;
        public String ruleOverrideSymbol;
        public ManualIndentation ruleOverride;

        /**
         * Since indentation requires functional toplevel, this is true if user is indenting already finished
         * element, ie doing it to a done element. If it is false, code is insering 0 and remaining braces to
         * form a functional element. This is important to tagbody.
         */
        public boolean hasRealElement = true;
    }

    protected interface IndentationImplementation {

        Integer calculateOffsetForForm(IndentationState state, LispElement topLevel, LispContainer container,
                                       Stack<IndentationBackTrack> backTrackStack) throws Exception;

    }

    // specific implementations here:

    private Integer lispIndentLoop(IndentationState state, LispElement topLevel, LispContainer container,
                           Stack<IndentationBackTrack> backTrackStack) throws Exception {
        // very simple implementation based on last symbol
        int offsetDedent = state.hasRealElement ? 0 : 1;
        if (state.hasRealElement) {
            if (container.getItems().size() == 0) {
                return state.settings.defaultIndentation + state.formIndentation.getOrDefault(container, OffsetInfo.DEFAULT).parentForm;
            }
        } else {
            if (container.getItems().size() < 2) {
                return state.settings.defaultIndentation + state.formIndentation.getOrDefault(container, OffsetInfo.DEFAULT).parentForm;
            }
        }

        int ix = container.getItems().size();
        LispElement checkedElement = container.getItems().get(ix - offsetDedent - 1);
        while (checkedElement instanceof LispSymbol symbol) {
            if (loopBodyForms.contains(symbol.getValue().toUpperCase())) {
                return state.settings.bodyIndentation + state.formIndentation.getOrDefault(container, OffsetInfo.DEFAULT).parentForm;
            }
            ix--;
            int pos = ix - offsetDedent - 1;
            if (pos < 0) {
                return 6 + state.formIndentation.getOrDefault(container, OffsetInfo.DEFAULT).parentForm;
            }
            checkedElement = container.getItems().get(pos);
        }

        return 6 + state.formIndentation.getOrDefault(container, OffsetInfo.DEFAULT).parentForm;
    }

    private Integer lispIndentDefsetf(IndentationState state, LispElement topLevel, LispContainer container,
                              Stack<IndentationBackTrack> backTrackStack) throws Exception {
        if (state.ruleOverride != null) {
            return null;
        }
        state.ruleOverrideSymbol = "DEFSETF";
        state.ruleOverride = specialIndentations.get("&DEFSETF");

        if (!container.getItems().isEmpty()) {
            LispContainer self = null;
            if (container.getItems().get(0) instanceof LispSymbol head) {
                if (head.getValue().equalsIgnoreCase("DEFSETF")) {
                    self = container;
                }
            }
            if (self == null) {
                self = backTrackStack.peek().parentContainer;
            }
            if (!(self.getItems().get(0) instanceof LispSymbol head && head.getValue().equalsIgnoreCase("DEFSETF")))
                return null;
            LispElement test = null;
            if (state.hasRealElement) {
                if (self.getItems().size() > 1) {
                    test = self.getItems().get(1);
                }
            } else {
                if (self.getItems().size() > 2) {
                    test = self.getItems().get(2);
                }
            }

            if (test != null) {
                if (test instanceof LispContainer) {
                    state.ruleOverride = specialIndentations.get("&DEFSETF-LONGER");
                }
            }
        }

        return calculateIndentForForm(state, topLevel, (LispContainer) topLevel, backTrackStack);
    }

    private Integer lispIndentDefmethod(IndentationState state, LispElement topLevel, LispContainer container,
                                Stack<IndentationBackTrack> backTrackStack) throws Exception {
        if (state.ruleOverride != null) {
            return null;
        }
        state.ruleOverrideSymbol = "DEFMETHOD";
        state.ruleOverride = specialIndentations.get("&DEFMETHOD");

        if (!container.getItems().isEmpty()) {
            LispContainer self = null;
            if (container.getItems().get(0) instanceof LispSymbol head) {
                if (head.getValue().equalsIgnoreCase("DEFMETHOD")) {
                    self = container;
                }
            }
            if (self == null) {
                self = getMethodBacktrack(backTrackStack);
            }
            if (self == null) {
                return null;
            }

            if (!(self.getItems().get(0) instanceof LispSymbol head && head.getValue().equalsIgnoreCase("DEFMETHOD")))
                return null;
            LispElement test = null;
            if (self.getItems().size() > 1) {
                test = self.getItems().get(1);
            }

            if (test != null) {
                if (!(test instanceof LispContainer)) {
                    if (test instanceof LispSymbol s) {
                        if (s.getValue().startsWith(":")) {
                            state.ruleOverride = specialIndentations.get("&DEFMETHOD-SPECIFIER");
                        }
                    }
                }
            }
        }

        return calculateIndentForForm(state, topLevel, (LispContainer) topLevel, backTrackStack);
    }

    private LispContainer getMethodBacktrack(Stack<IndentationBackTrack> backTrackStack) {
        for (int ix=backTrackStack.size()-1; ix>=0; ix--) {
            IndentationBackTrack backTrack = backTrackStack.get(ix);
            if (backTrack.parentContainer.getItems().size() > 0) {
                if (backTrack.parentContainer.getItems().get(0) instanceof LispSymbol s && s.getValue().equalsIgnoreCase("DEFMETHOD"))
                    return backTrack.parentContainer;
            }
        }
        return null;
    }

    private Integer lispIndentTagbody(IndentationState state, LispElement topLevel, LispContainer container,
                              Stack<IndentationBackTrack> backTrackStack) throws Exception {
        if (container.getItems().size() == 0) {
            return state.settings.bodyIndentation;
        }

        List<LispElement> sublist = container.getItems();
        if (container.getItems().get(0) instanceof LispSymbol s && (
                s.getValue().equalsIgnoreCase("do") || s.getValue().equalsIgnoreCase("do*"))) {
            List<LispElement> sublist2 = new ArrayList<>();
            sublist2.add(new LispSymbol("TAGBODY"));
            sublist2.addAll(sublist.subList(2, sublist.size()));
            sublist = sublist2;
        }

        if (state.hasRealElement) {
            if (sublist.size() < 1) {
                return state.settings.bodyIndentation;
            }
            LispElement lastElement = sublist.get(sublist.size() - 1);
            if (lastElement instanceof LispContainer) {
                return state.settings.bodyIndentation;
            } else {
                return state.settings.tagbodyIndentation;
            }
        } else {
            if (sublist.size() < 2) {
                return state.settings.bodyIndentation;
            }
            LispElement lastElement = sublist.get(sublist.size() - 2);
            if (lastElement instanceof LispContainer) {
                return state.settings.tagbodyIndentation;
            } else {
                return state.settings.bodyIndentation;
            }
        }
    }

}

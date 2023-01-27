package com.en_circle.slt.tests.indentation;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltCommonLispParserDefinition;
import com.en_circle.slt.plugin.indentation.SltIndentationSettings;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils.OffsetInfo;
import com.en_circle.slt.plugin.lisp.psi.LispCoreProjectEnvironment;
import com.en_circle.slt.plugin.services.lisp.components.SltIndentationContainer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.Stack;

public class TestableSltIndentationContainer extends SltIndentationContainer {

    private final LispCoreProjectEnvironment projectEnvironment;

    public TestableSltIndentationContainer() {
        projectEnvironment = new LispCoreProjectEnvironment();
        projectEnvironment.getEnvironment()
                .registerParserDefinition(SltCommonLispLanguage.INSTANCE, new SltCommonLispParserDefinition());
    }

    public Project getProject() {
        return projectEnvironment.getProject();
    }

    public Integer testIndentCL(String formText) throws Exception {
        IndentationState state = new IndentationState();
        state.project = getProject();
        state.hasRealElement = false;
        state.formIndentation = new IdentityHashMap<>();
        state.packageName = "COMMON-LISP-USER";
        state.settings = new SltIndentationSettings();

        PsiFileFactory factory = PsiFileFactory.getInstance(getProject());
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

}

package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.lisp.psi.LispSexpr;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.intellij.icons.AllIcons.Nodes;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.breakpoints.XBreakpoint;
import com.intellij.xdebugger.breakpoints.XLineBreakpointType;
import com.intellij.xdebugger.impl.XSourcePositionImpl;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.*;

public class SltSymbolBreakpointType extends XLineBreakpointType<SltBreakpointProperties> {

    protected SltSymbolBreakpointType() {
        super(SltSymbolBreakpointType.class.getName(), SltBundle.message("slt.ui.debugger.breakpoint"));
    }

    @Override
    public boolean canPutAt(@NotNull VirtualFile file, int line, @NotNull Project project) {
        if (file.getFileType() == SltCommonLispFileType.INSTANCE)
            return getPossiblePsiDebugSymbols(file, line, project).size() > 0;
        return false;
    }

    @Override
    public @Nullable SltBreakpointProperties createBreakpointProperties(@NotNull VirtualFile file, int line) {
        return null;
    }

    @Override
    @Nullable
    public SltBreakpointProperties createProperties() {
        return new SltBreakpointProperties();
    }

    private List<SltBreakpointInfo> getPossiblePsiDebugSymbols(VirtualFile file, int line, Project project) {
        List<SltBreakpointInfo> infoList = new ArrayList<>();

        PsiFile f = PsiManager.getInstance(project).findFile(file);
        if (f != null) {
            Document document = PsiDocumentManager.getInstance(project).getDocument(f);
            if (document != null) {
                int offset = document.getLineStartOffset(line);
                int endoffset = document.getLineEndOffset(line);
                Set<LispToplevel> checkedToplevels = new HashSet<>();
                Set<PsiElement> checkedElements = new HashSet<>();

                while (offset < endoffset) {
                    PsiElement lineElement = getValidElement(offset++, f);
                    if (lineElement != null) {
                        if (checkedElements.contains(lineElement))
                            continue;
                        checkedElements.add(lineElement);

                        LispToplevel toplevel = PsiTreeUtil.getTopmostParentOfType(lineElement, LispToplevel.class);
                        if (toplevel != null) {
                            if (checkedToplevels.contains(toplevel))
                                continue;
                            checkedToplevels.add(toplevel);

                            LispList list = PsiTreeUtil.getParentOfType(lineElement, LispList.class);
                            do {
                                if (list != null) {
                                    List<LispSexpr> sexprs = list.getSexprList();
                                    if (sexprs.size() > 0) {
                                        if (sexprs.get(0).getDatum() != null &&
                                                Objects.requireNonNull(sexprs.get(0).getDatum()).getCompoundSymbol() != null) {
                                            LispSymbol head = Objects.requireNonNull(Objects.requireNonNull(sexprs.get(0).getDatum()).getCompoundSymbol()).getSymbol();

                                            if ("defun".equalsIgnoreCase(head.getName())) {
                                                if (sexprs.size() > 1) {
                                                    addIfSymbol(sexprs.get(1), infoList, offset, endoffset, head.getName());
                                                }
                                            }
                                        }
                                    }
                                    list = PsiTreeUtil.getParentOfType(list, LispList.class);
                                }
                            } while (list != null);
                        }
                    }
                }
            }
        }

        return infoList;
    }

    private void addIfSymbol(LispSexpr sexpr, List<SltBreakpointInfo> infoList, int offset, int endoffset,
                             String headName) {
        if (sexpr.getDatum() != null) {
            if (sexpr.getDatum().getCompoundSymbol() != null) {
                LispSymbol symbol = sexpr.getDatum().getCompoundSymbol().getSymbol();
                int soffset = symbol.getTextOffset();
                if (soffset >= offset && soffset <= endoffset) {
                    String packageName = LispParserUtil.getPackage(symbol);
                    SltBreakpointInfo info = new SltBreakpointInfo(symbol, packageName);
                    infoList.add(info);
                }
            }
        }
    }

    private PsiElement getValidElement(int offset, PsiFile f) {
        PsiElement element = f.findElementAt(offset);
        while (element != null) {
            if (element instanceof PsiWhiteSpace) {
                element = f.findElementAt(++offset);
                continue;
            }
            return element;
        }
        return null;
    }

    @Override
    public XSourcePosition getSourcePosition(@NotNull XBreakpoint<SltBreakpointProperties> breakpoint) {
        SltBreakpointProperties properties = breakpoint.getProperties();
        VirtualFile virtualFile = VirtualFileManager.getInstance().findFileByUrl(properties.file);
        return XSourcePositionImpl.createByOffset(virtualFile, properties.offset);
    }

    @Override
    public @NotNull List<? extends XLineBreakpointType<SltBreakpointProperties>.XLineBreakpointVariant>
                        computeVariants(@NotNull Project project, @NotNull XSourcePosition position) {
        return getPossiblePsiDebugSymbols(position.getFile(), position.getLine(), project);
    }

    private class SltBreakpointInfo extends XLineBreakpointVariant {

        private final LispSymbol element;
        private final String packageName;

        private SltBreakpointInfo(@NotNull LispSymbol element, String packageName) {
            this.element = element;
            this.packageName = packageName;
        }

        @Override
        public @NotNull @Nls String getText() {
            return Objects.requireNonNull(element.getName());
        }

        @Override
        public @Nullable Icon getIcon() {
            return Nodes.Function;
        }

        @Override
        public @Nullable TextRange getHighlightRange() {
            return element.getTextRange();
        }

        @Override
        public @Nullable SltBreakpointProperties createProperties() {
            SltBreakpointProperties properties = new SltBreakpointProperties();

            properties.symbolName = element.getName();
            properties.packageName = packageName;
            properties.file = element.getContainingFile().getVirtualFile().getUrl();
            properties.offset = element.getTextOffset();

            return properties;
        }
    }

}

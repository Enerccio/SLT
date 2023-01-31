package com.en_circle.slt.plugin.references;

import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.en_circle.slt.plugin.swank.requests.Xrefs;
import com.en_circle.slt.plugin.swank.requests.Xrefs.XrefType;
import com.en_circle.slt.tools.LocationUtils;
import com.en_circle.slt.tools.SltApplicationUtils;
import com.intellij.lang.ASTNode;
import com.intellij.lang.FileASTNode;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class SltReference extends PsiPolyVariantReferenceBase<LispSymbol> {

    private static SltReferenceResolveCache cache = new SltReferenceResolveCache();

    private static final com.en_circle.slt.plugin.lisp.lisp.LispSymbol locationAccessor =
            new com.en_circle.slt.plugin.lisp.lisp.LispSymbol(":LOCATION");

    public SltReference(@NotNull LispSymbol element) {
        super(element, true);
    }

    @Override
    public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
        PsiElement element = getElement();
        return cache.resolve(this);
    }

    ResolveResult[] resolveInner(boolean incompleteCode) {
        PsiManager manager = PsiManager.getInstance(myElement.getProject());

        String symbolName = myElement.getName();
        String packageName = LispParserUtil.getPackage(myElement);
        SymbolState state = LispEnvironmentService.getInstance(myElement.getProject()).refreshSymbolFromServer(packageName, symbolName, myElement);
        SourceLocation location = state.location;
        LispToplevel topLevel = PsiTreeUtil.getParentOfType(myElement, LispToplevel.class);

        if (location.isFile()) {
            VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(new File(location.getLocation()));
            if (vf != null) {
                PsiFile file = PsiManager.getInstance(myElement.getProject()).findFile(vf);
                if (file != null) {
                    FileASTNode node = file.getNode();
                    if (node != null) {
                        ASTNode reference = node.findLeafElementAt(location.getPosition());
                        if (reference != null) {
                            PsiElement referenceElement = reference.getPsi();
                            if (referenceElement != null) {
                                LispToplevel selfTopLevel = PsiTreeUtil.getParentOfType(referenceElement, LispToplevel.class);
                                if (topLevel != null && selfTopLevel != null) {
                                    if (manager.areElementsEquivalent(selfTopLevel, topLevel)) {
                                        return resolveReferencesTo(symbolName, packageName, state);
                                    }
                                }
                                return getReferenceClosesSymbolOrFail(referenceElement, myElement.getName());
                            }
                        }
                    }
                }
            }
        }
        return resolveReferencesTo(symbolName, packageName, state);
    }

    private ResolveResult[] getReferenceClosesSymbolOrFail(PsiElement referenceElement, String name) {
        LispSymbol ls = PsiTreeUtil.getParentOfType(referenceElement, LispSymbol.class);
        if (ls != null && ls.getName() != null && ls.getName().equalsIgnoreCase(name)) {
            return new ResolveResult[] { new PsiElementResolveResult(ls) };
        }

        PsiElement parent = PsiTreeUtil.getParentOfType(referenceElement, LispToplevel.class);
        if (parent != null) {
            Collection<LispSymbol> symbols = PsiTreeUtil.findChildrenOfType(parent, LispSymbol.class);
            for (LispSymbol symbol : symbols) {
                if (symbol.getName() != null && symbol.getName().equalsIgnoreCase(name)) {
                    return new ResolveResult[] { new PsiElementResolveResult(symbol) };
                }
            }
        }

        if (referenceElement instanceof LispSymbol) {
            // default just move to this even if it is a different symbol
            return new ResolveResult[] { new PsiElementResolveResult(referenceElement) };
        }
        return new ResolveResult[0];
    }

    @SuppressWarnings("DuplicateBranchesInSwitch")
    private ResolveResult[] resolveReferencesTo(String symbolName, String packageName, SymbolState state) {
        List<XrefType> applicableTypes = new ArrayList<>();

        switch (state.binding) {
            case NONE -> applicableTypes.add(XrefType.REFERENCES);
            case FUNCTION -> applicableTypes.add(XrefType.CALLS);
            case MACRO -> applicableTypes.add(XrefType.MACRO_EXPANDS);
            case SPECIAL_FORM -> {

            }
            case CONSTANT -> applicableTypes.add(XrefType.REFERENCES);
            case KEYWORD -> {

            }
            case SPECIAL_VARIABLE -> {
                applicableTypes.add(XrefType.BINDS);
                applicableTypes.add(XrefType.REFERENCES);
            }
            case CLASS -> {
                applicableTypes.add(XrefType.REFERENCES);
                applicableTypes.add(XrefType.SPECIALIZES);
            }
            case METHOD -> {
                applicableTypes.add(XrefType.CALLS);
                applicableTypes.add(XrefType.SPECIALIZES);
            }
        }

        if (applicableTypes.size() > 0) {
            try {
                ResolveResult[] results = SltApplicationUtils.getAsyncResultCheckCancellation(myElement.getProject(),
                        finishRequest -> Xrefs.xrefs(symbolName, packageName, applicableTypes, form ->
                                finishRequest.accept(gatherReferences(form))), false);
                if (results != null) {
                    return results;
                }
            } catch (Exception ignored) {

            }
        }
        return new ResolveResult[0];
    }

    private ResolveResult[] gatherReferences(LispElement form) {
        try {
            List<ResolveResult> results = new ArrayList<>();
            if (form instanceof LispContainer flist) {
                for (LispElement type : flist.getItems()) {
                    if (type instanceof LispContainer typeContainer) {
                        if (typeContainer.getItems().size() > 1) {
                            for (LispElement data : typeContainer.getItems()) {
                                if (data instanceof LispContainer d) {
                                    if (d.getItems().size() > 1 && d.getItems().get(1) instanceof LispContainer c) {
                                        d = c;
                                    }
                                    if (LispUtils.hasPValue(d, locationAccessor)) {
                                        SourceLocation sourceLocation = new SourceLocation(d);
                                        ResolveResult result = convertLocationToReference(sourceLocation, myElement.getName());
                                        if (result != null) {
                                            results.add(result);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return results.toArray(new ResolveResult[0]);
        } catch (Exception e) {
            return new ResolveResult[0];
        }
    }

    private ResolveResult convertLocationToReference(SourceLocation location, String name) {
        return LocationUtils.convertFromLocationToSymbol(myElement.getProject(), location, name,
                PsiElementResolveResult::new);
    }

    @Override
    public PsiElement resolve() {
        ResolveResult[] results = multiResolve(false);
        return results.length == 1 ? results[0].getElement() : null;
    }

    @Override
    public boolean isReferenceTo(@NotNull final PsiElement element) {
        return getElement().getManager().areElementsEquivalent(resolve(), element);
    }

    @Override
    public boolean isSoft(){
        return false;
    }

}

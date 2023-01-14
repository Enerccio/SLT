package com.en_circle.slt.plugin.references;

import com.en_circle.slt.plugin.SltSBCL;
import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.intellij.lang.ASTNode;
import com.intellij.lang.FileASTNode;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;

public class SltReference extends PsiReferenceBase<LispSymbol> implements
        PsiPolyVariantReference, ResolvingHint {

    public SltReference(@NotNull LispSymbol element) {
        super(element);
    }

    @Override
    public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
        String symbolName = myElement.getName();
        String packageName = LispParserUtil.getPackage(myElement);
        SymbolState state = SltSBCL.getInstance().refreshSymbolFromServer(packageName, symbolName, myElement);
        SourceLocation location = state.location;
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
                                ResolveResult resolveResult = new ResolveResult() {
                                    @Override
                                    public @Nullable PsiElement getElement() {
                                        return referenceElement;
                                    }

                                    @Override
                                    public boolean isValidResult() {
                                        return true;
                                    }
                                };
                                return new ResolveResult[] { resolveResult };
                            }
                        }
                    }
                }
            }
        }
        return new ResolveResult[0];
    }

    @Override
    public @Nullable PsiElement resolve() {
        ResolveResult[] resolveResults = multiResolve(false);
        return resolveResults.length == 1 ? resolveResults[0].getElement() : null;
    }

    @Override
    public boolean canResolveTo(Class<? extends PsiElement> elementClass) {
        return elementClass.isAssignableFrom(LispSymbol.class);
    }
}

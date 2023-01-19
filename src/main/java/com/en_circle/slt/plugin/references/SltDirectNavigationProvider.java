package com.en_circle.slt.plugin.references;

import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.lisp.LispParserUtil;
import com.en_circle.slt.plugin.lisp.psi.LispSymbol;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.intellij.lang.ASTNode;
import com.intellij.lang.FileASTNode;
import com.intellij.navigation.DirectNavigationProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;

public class SltDirectNavigationProvider implements DirectNavigationProvider {

    @Override
    public @Nullable PsiElement getNavigationElement(@NotNull PsiElement element) {
        if (element instanceof LispSymbol) {
            String packageName = LispParserUtil.getPackage(element);
            Project project = element.getProject();
            String symbolName = ((LispSymbol) element).getName();
            SymbolState state = LispEnvironmentService.getInstance(element.getProject()).refreshSymbolFromServer(packageName, symbolName, element);
            SourceLocation location = state.location;
            if (location.isFile()) {
                VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(new File(location.getLocation()));
                if (vf != null) {
                    PsiFile file = PsiManager.getInstance(project).findFile(vf);
                    if (file != null) {
                        FileASTNode node = file.getNode();
                        if (node != null) {
                            ASTNode reference = node.findLeafElementAt(location.getPosition());
                            if (reference != null) {
                                return reference.getPsi();
                            }
                        }
                    }
                }
            }
        }
        return null;
    }
}

package com.en_circle.slt.tools;

import com.en_circle.slt.plugin.references.SltFakePsiElement;
import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiNamedElement;

import java.io.File;
import java.util.function.Function;

public class LocationUtils {

    public static <X> X convertFromLocationToSymbol(Project project, SourceLocation location, String name,
                                                    Function<PsiNamedElement, X> converter) {
        if (location.isFile()) {
            VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(new File(location.getLocation()));
            if (vf != null) {
                PsiFile f = PsiManager.getInstance(project).findFile(vf);
                if (f != null) {
                    return converter.apply(new SltFakePsiElement(f, name, location.getPosition()));
                }
            }
        }
        return null;
    }

}

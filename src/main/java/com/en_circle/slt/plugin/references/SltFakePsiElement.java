package com.en_circle.slt.plugin.references;

import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.intellij.ide.util.PsiNavigationSupport;
import com.intellij.lang.Language;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.pom.Navigatable;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.FakePsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class SltFakePsiElement extends FakePsiElement implements NavigatablePsiElement {

    private final PsiFile f;
    private final int position;
    private final String name;
    private final ItemPresentation presentation;

    public SltFakePsiElement(PsiFile f, String name, int position) {
        this.f = f;
        this.name = name;
        this.position = position;
        this.presentation = new ItemPresentation() {
            @Override
            public @NlsSafe @Nullable String getPresentableText() {
                return name;
            }

            @Override
            public @Nullable Icon getIcon(boolean unused) {
                return null;
            }

        };
    }

    @Override
    public @NotNull Project getProject() {
        return f.getProject();
    }

    @Override
    public PsiElement getParent() {
        return f;
    }

    @Override
    public int getTextOffset() {
        return position;
    }

    @Override
    public int getStartOffsetInParent() {
        return position;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public ItemPresentation getPresentation() {
        return presentation;
    }

    @Override
    public @NotNull Language getLanguage() {
        return SltCommonLispLanguage.INSTANCE;
    }

    @Override
    public void navigate(boolean requestFocus) {
        Navigatable descriptor = PsiNavigationSupport.getInstance().getDescriptor(this);
        if (descriptor != null) {
            descriptor.navigate(requestFocus);
        }
    }

    @Override
    public boolean canNavigate() {
        return true;
    }
}

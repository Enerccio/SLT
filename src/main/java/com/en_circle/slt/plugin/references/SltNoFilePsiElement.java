package com.en_circle.slt.plugin.references;

import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.intellij.lang.Language;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.FakePsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class SltNoFilePsiElement extends FakePsiElement implements NavigatablePsiElement {
    private final String name;
    private final ItemPresentation presentation;
    private final Project project;

    public SltNoFilePsiElement(Project project, String name) {
        this.name = name;
        this.project = project;
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
        return project;
    }

    @Override
    public PsiElement getParent() {
        return null;
    }

    @Override
    public PsiFile getContainingFile() {
        return null;
    }

    @Override
    public int getTextOffset() {
        return -1;
    }

    @Override
    public int getStartOffsetInParent() {
        return -1;
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

    }

    @Override
    public boolean canNavigate() {
        return false;
    }
}

package com.en_circle.slt.plugin.structure;

import com.en_circle.slt.plugin.lisp.psi.LispToplevel;
import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.StructureViewModelBase;
import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.util.treeView.smartTree.Sorter;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

public class LispStructureViewModel extends StructureViewModelBase implements
        StructureViewModel.ElementInfoProvider {

    public LispStructureViewModel(PsiFile psiFile, Editor editor) {
        super(psiFile, editor, new LispStructureViewElement(psiFile, null));
    }

    @NotNull
    public Sorter @NotNull [] getSorters() {
        return new Sorter[]{Sorter.ALPHA_SORTER};
    }

    @Override
    public boolean isAlwaysLeaf(StructureViewTreeElement element) {
        return element.getValue() instanceof LispToplevel;
    }

    @Override
    public boolean isAlwaysShowsPlus(StructureViewTreeElement element) {
        return false;
    }

    @Override
    protected Class<?> @NotNull [] getSuitableClasses() {
        return new Class[]{ LispToplevel.class };
    }
}

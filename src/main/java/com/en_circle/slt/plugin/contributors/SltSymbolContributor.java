package com.en_circle.slt.plugin.contributors;

import com.en_circle.slt.plugin.environment.LispFeatures;
import com.en_circle.slt.plugin.lisp.LispPotentialSymbolPresentation;
import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.en_circle.slt.plugin.swank.requests.CompleteSearch;
import com.en_circle.slt.plugin.swank.requests.CompleteSearch.SearchFilter;
import com.en_circle.slt.tools.LocationUtils;
import com.en_circle.slt.tools.SltApplicationUtils;
import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.navigation.ItemPresentation;
import com.intellij.navigation.NavigationItem;
import com.intellij.navigation.PsiElementNavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class SltSymbolContributor implements ChooseByNameContributor {

    protected SearchFilter getFilter() {
        return null;
    }

    @Override
    public String @NotNull [] getNames(Project project, boolean includeNonProjectItems) {
        String[] names = null;
        if (LispEnvironmentService.getInstance(project).hasFeature(LispFeatures.SEARCH)) {
            try {
                names = SltApplicationUtils.getAsyncResultCheckCancellation(project,
                        finishRequest -> CompleteSearch.search("", getFilter(), form ->
                                finishRequest.accept(getNames(form, includeNonProjectItems, project))), false);
            } catch (Exception e) {
                return new String[0];
            }
        }

        if (names == null) {
            return new String[0];
        }
        return names;
    }

    private String[] getNames(LispElement form, boolean includeNonProjectItems, Project project) {
        List<String> classNames = new ArrayList<>();
        if (form instanceof LispContainer container) {
            for (LispElement inner : container.getItems()) {
                if (inner instanceof LispContainer c) {
                    String name = ((LispString) c.getItems().get(0)).getValue();
                    if (includeNonProjectItems) {
                        classNames.add(name);
                    } else {
                        LispElement location = c.getItems().get(3);
                        if (location instanceof LispContainer loc) {
                            SourceLocation l = new SourceLocation(loc);
                            if (l.isPrecise()) {
                                File fl = new File(l.getLocation());
                                VirtualFile vf = VirtualFileManager.getInstance().findFileByNioPath(fl.toPath());
                                if (vf != null) {
                                    ProjectFileIndex index = ProjectFileIndex.getInstance(project);
                                    if (index.isInSource(vf)) {
                                        classNames.add(name);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return classNames.toArray(new String[0]);
    }

    @Override
    public NavigationItem @NotNull [] getItemsByName(String name, String pattern, Project project, boolean includeNonProjectItems) {
        NavigationItem[] items;
        try {
            items = SltApplicationUtils.getAsyncResultCheckCancellation(project,
                    finishRequest -> CompleteSearch.search(pattern, getFilter(), form ->
                            finishRequest.accept(getItems(form, includeNonProjectItems, project))), false);
        } catch (Exception e) {
            return new NavigationItem[0];
        }

        if (items == null) {
            return new NavigationItem[0];
        }
        return items;
    }

    private NavigationItem[] getItems(LispElement form, boolean includeNonProjectItems, Project project) {
        List<NavigationItem> navigations = new ArrayList<>();
        if (form instanceof LispContainer container) {
            for (LispElement inner : container.getItems()) {
                if (inner instanceof LispContainer c) {
                    if (includeNonProjectItems) {
                        navigations.add(createNavigationElement(project, c));
                    } else {
                        LispElement location = c.getItems().get(3);
                        if (location instanceof LispContainer loc) {
                            SourceLocation l = new SourceLocation(loc);
                            if (l.isPrecise()) {
                                File fl = new File(l.getLocation());
                                VirtualFile vf = VirtualFileManager.getInstance().findFileByNioPath(fl.toPath());
                                if (vf != null) {
                                    ProjectFileIndex index = ProjectFileIndex.getInstance(project);
                                    if (index.isInSource(vf)) {
                                        navigations.add(createNavigationElement(project, c));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return navigations.stream().filter(Objects::nonNull).toArray(NavigationItem[]::new);
    }

    private NavigationItem createNavigationElement(Project project, LispContainer c) {
        String name = ((LispString) c.getItems().get(0)).getValue();
        String packageName = null;
        if (c.getItems().get(1) instanceof LispString pkgName)
            packageName = pkgName.getValue();
        final String packageNameFinal = packageName;
        LispElement location = c.getItems().get(3);
        if (location instanceof LispContainer loc) {
            SourceLocation l = new SourceLocation(loc);
            return LocationUtils.convertFromLocationToSymbol(project, l, name,
                    s -> mkElement(project, s, packageNameFinal, l.getPosition()));
        }

        return null;
    }

    private NavigationItem mkElement(Project project, PsiNamedElement symbol, String packageName, int offset) {
        return new PsiElementNavigationItem() {
            @Override
            public @Nullable PsiElement getTargetElement() {
                return symbol;
            }

            @Override
            public @Nullable @NlsSafe String getName() {
                return symbol.getName();
            }

            @Override
            public @Nullable ItemPresentation getPresentation() {
                return new LispPotentialSymbolPresentation(project, symbol.getContainingFile(),
                        symbol.getName(), packageName, offset);
            }

            @Override
            public void navigate(boolean requestFocus) {
                ((NavigatablePsiElement) symbol).navigate(requestFocus);
            }

            @Override
            public boolean canNavigate() {
                return ((NavigatablePsiElement) symbol).canNavigate();
            }

            @Override
            public boolean canNavigateToSource() {
                return true;
            }
        };
    }
}

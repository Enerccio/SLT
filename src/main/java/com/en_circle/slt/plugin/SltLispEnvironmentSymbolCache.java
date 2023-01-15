package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.SymbolState.SymbolBinding;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.en_circle.slt.plugin.swank.requests.EvalAndGrab;
import com.google.common.collect.Lists;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.util.FileContentUtilCore;
import org.apache.commons.lang3.StringUtils;

import java.lang.ref.WeakReference;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class SltLispEnvironmentSymbolCache extends Thread {

    public static final SltLispEnvironmentSymbolCache INSTANCE = new SltLispEnvironmentSymbolCache();
    static {
        INSTANCE.start();
    }

    private final Map<String, SymbolState> symbolInformation = Collections.synchronizedMap(new HashMap<>());
    private final List<SymbolState> symbolRefreshQueue = Collections.synchronizedList(new ArrayList<>());

    private SltLispEnvironmentSymbolCache() {
        setDaemon(true);
        setName("SBCL Symbol Cache Thread");
    }

    @Override
    public void run() {
        while (true) {
            try {
                if (SltLispEnvironmentProvider.getInstance().isLispEnvironmentActive()) {
                    while (symbolRefreshQueue.isEmpty()) {
                        Thread.sleep(1000);
                    }
                    List<SymbolState> refreshStates;
                    synchronized (symbolRefreshQueue) {
                        refreshStates = new ArrayList<>(symbolRefreshQueue);
                        symbolRefreshQueue.clear();
                    }
                    System.err.println("Refreshing " + refreshStates.size() + " symbols");
                    try {
                        refreshSymbols(refreshStates);
                    } catch (Exception e) {
                        // ignored
                    }
                }
                Thread.sleep(100);
            } catch (InterruptedException exception) {

            }
        }
    }

    public SymbolState refreshSymbolFromServer(String packageName, String symbolName, PsiElement element) {
        SymbolState state = getOrCreateBinding(packageName, symbolName);
        SymbolState undefinedSymbol = getOrCreateBinding(null, symbolName);
        state.containerFiles.add(new WeakReference<>(element.getContainingFile().getVirtualFile()));
        SymbolBinding currentBinding = state.binding;
        if (currentBinding == SymbolBinding.NONE) {
            symbolRefreshQueue.add(state);
            if (undefinedSymbol.binding == SymbolBinding.NONE) {
                symbolRefreshQueue.add(undefinedSymbol);
            } else {
                return undefinedSymbol;
            }
        }

        return state;
    }

    private SymbolState getOrCreateBinding(String packageName, String symbolName) {
        if (symbolName.contains("::")) {
            String[] parts = symbolName.split(Pattern.quote("::"));
            if (parts.length > 1) {
                if (StringUtils.isBlank(parts[0])) {
                    packageName = null;
                } else {
                    packageName = parts[0];
                    symbolName = parts[1];
                }
            }
        } else if (symbolName.contains(":")) {
            String[] parts = symbolName.split(Pattern.quote(":"));
            if (parts.length > 1) {
                if (StringUtils.isBlank(parts[0])) {
                    packageName = null;
                } else {
                    packageName = parts[0];
                    symbolName = parts[1];
                }
            }
        }
        String combinedName = StringUtils.isBlank(packageName) ? symbolName : (packageName + ":" + symbolName);
        String pureName = symbolName;
        String packageNameFinal = packageName;
        return symbolInformation.computeIfAbsent(combinedName, fullname -> new SymbolState(fullname, packageNameFinal, pureName));
    }

    private void refreshSymbols(List<SymbolState> refreshStates) throws Exception {
        HashSet<SymbolState> duplicityState = new HashSet<>();
        List<SymbolState> withoutDuplicity = refreshStates.stream()
                        .filter(s -> {
                            boolean isDuplicit = duplicityState.contains(s);
                            duplicityState.add(s);
                            return !isDuplicit;
                        }).filter(this::removeBad).collect(Collectors.toList());
        Lists.partition(withoutDuplicity, 500).forEach(list -> {
                    try {
                        refreshSymbolsBatched(list);
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                });
    }

    private boolean removeBad(SymbolState symbolState) {
        return !symbolState.symbolName.contains(":");
    }

    private void refreshSymbolsBatched(List<SymbolState> refreshStates) throws Exception {
        String request = "(" +
                refreshStates.stream().map(x -> x.name.toUpperCase() + " ").collect(Collectors.joining()) + ")";
        request = StringUtils.replace(request, "\"", "\\\"");

        SltLispEnvironmentProvider.getInstance().sendToLisp(EvalAndGrab.eval(
                String.format(
                        "(slt-core:analyze-symbols (slt-core:read-fix-packages \"%s\"))",
                        request),
                SltLispEnvironmentProvider.getInstance().getGlobalPackage(), true, (result, stdout, parsed) -> {
                    Set<VirtualFile> toRefresh = new HashSet<>();
                    if (parsed.size() == 1 && parsed.get(0).getType() == LispElementType.CONTAINER) {
                        int ix = 0;
                        LispContainer data = (LispContainer) parsed.get(0);
                        assert refreshStates.size() == data.getItems().size();
                        for (LispElement element : data.getItems()) {
                            LispContainer list = (LispContainer) element;
                            String name = ((LispSymbol) list.getItems().get(0)).getValue().toUpperCase();
                            if ("NIL".equals(name)) {
                                ++ix;
                                continue;
                            }

                            SymbolState state = refreshStates.get(ix++);
                            if (state != null) {
                                String symValue = ((LispSymbol) list.getItems().get(1)).getValue().toUpperCase();
                                boolean changed = false;
                                state.timestamp = System.currentTimeMillis();
                                switch (symValue) {
                                    case ":CLASS":
                                        changed |= state.binding != SymbolBinding.CLASS;
                                        state.binding = SymbolBinding.CLASS;
                                        break;
                                    case ":SPECIAL-FORM":
                                        changed |= state.binding != SymbolBinding.SPECIAL_FORM;
                                        state.binding = SymbolBinding.SPECIAL_FORM;
                                        break;
                                    case ":MACRO":
                                        changed |= state.binding != SymbolBinding.MACRO;
                                        state.binding = SymbolBinding.MACRO;
                                        break;
                                    case ":FUNCTION":
                                        changed |= state.binding != SymbolBinding.FUNCTION;
                                        state.binding = SymbolBinding.FUNCTION;
                                        break;
                                    case ":CONSTANT":
                                        changed |= state.binding != SymbolBinding.CONSTANT;
                                        state.binding = SymbolBinding.CONSTANT;
                                        break;
                                    case ":KEYWORD":
                                        changed |= state.binding != SymbolBinding.KEYWORD;
                                        state.binding = SymbolBinding.KEYWORD;
                                        break;
                                    case ":SPECIAL":
                                        changed |= state.binding != SymbolBinding.SPECIAL_VARIABLE;
                                        state.binding = SymbolBinding.SPECIAL_VARIABLE;
                                        break;
                                    default:
                                        changed |= state.binding != SymbolBinding.NONE;
                                        state.binding = SymbolBinding.NONE;
                                        break;
                                }
                                boolean hasDoc = state.documentation != null;
                                state.documentation = null;
                                if (list.getItems().get(2) instanceof LispString) {
                                    state.documentation = ((LispString) list.getItems().get(2)).getValue();
                                    if (!hasDoc) {
                                        changed = true;
                                    }
                                }

                                if (list.getItems().get(3) instanceof LispContainer) {
                                    SourceLocation location = new SourceLocation((LispContainer) list.getItems().get(3));
                                    if (!state.location.equals(location)) {
                                        state.location = location;
                                        changed = true;
                                    }
                                }

                                if (changed) {
                                    for (WeakReference<VirtualFile> wvf : state.containerFiles) {
                                        VirtualFile vf = wvf.get();
                                        if (vf != null) {
                                            toRefresh.add(vf);
                                        }
                                    }
                                    state.containerFiles.clear();
                                }
                            }
                        }
                    }

                    if (!toRefresh.isEmpty()) {
                        ApplicationManager.getApplication().invokeLater(() -> {
                            for (VirtualFile vf : toRefresh) {
                                FileContentUtilCore.reparseFiles(vf);
                            }
                        });
                    }
                }), false);
    }

    public void clear() {
        symbolInformation.clear();
        symbolRefreshQueue.clear();
    }
}

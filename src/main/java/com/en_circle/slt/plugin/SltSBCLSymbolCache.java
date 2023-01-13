package com.en_circle.slt.plugin;

import com.en_circle.slt.plugin.SymbolState.SymbolBinding;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.swank.SwankServer;
import com.en_circle.slt.plugin.swank.requests.SwankEvalAndGrab;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.util.FileContentUtilCore;
import org.codehaus.plexus.util.StringUtils;

import java.lang.ref.WeakReference;
import java.util.*;
import java.util.stream.Collectors;

public class SltSBCLSymbolCache extends Thread {

    public static final SltSBCLSymbolCache INSTANCE = new SltSBCLSymbolCache();
    static {
        INSTANCE.start();
    }

    private final Map<String, SymbolState> symbolInformation = Collections.synchronizedMap(new HashMap<>());
    private final List<SymbolState> symbolRefreshQueue = Collections.synchronizedList(new ArrayList<>());

    private SltSBCLSymbolCache() {
        setDaemon(true);
        setName("SBCL Symbol Cache Thread");
    }

    @Override
    public void run() {
        while (true) {
            try {
                if (SwankServer.INSTANCE.isActive()) {
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
        state.containerFiles.add(new WeakReference<>(element.getContainingFile().getVirtualFile()));
        SymbolBinding currentBinding = state.binding;
        if (currentBinding == SymbolBinding.NONE || cacheInvalid(state)) {
            symbolRefreshQueue.add(state);
        }
        return state;
    }

    private boolean cacheInvalid(SymbolState state) {
        if (state.timestamp == null)
            return false;
//        if (System.currentTimeMillis() - state.timestamp > 10000)
//            return false;

        return true;
    }

    private SymbolState getOrCreateBinding(String packageName, String symbolName) {
        return symbolInformation.computeIfAbsent(packageName + ":" + symbolName, fullname -> new SymbolState(fullname, symbolName));
    }

    private void refreshSymbols(List<SymbolState> refreshStates) throws Exception {
        Map<String, SymbolState> stateMap = new HashMap<>();
        for (SymbolState state : refreshStates) {
            stateMap.put(state.symbolName.toUpperCase(), state);
        }
        String request = "(" +
                stateMap.keySet().stream().map(x -> x + " ").collect(Collectors.joining()) + ")";
        request = StringUtils.replace(request, "\"", "\\\"");

        SltSBCL.getInstance().sendToSbcl(SwankEvalAndGrab.eval(
                String.format(
                        "(slt-core:analyze-symbols (slt-core:read-fix-packages \"%s\"))",
                        request),
                SltSBCL.getInstance().getGlobalPackage(), true, (result, stdout, parsed) -> {
                    Set<VirtualFile> toRefresh = new HashSet<>();

                    if (parsed.size() == 1 && parsed.get(0).getType() == LispElementType.CONTAINER) {
                        LispContainer data = (LispContainer) parsed.get(0);
                        for (LispElement element : data.getItems()) {
                            LispContainer list = (LispContainer) element;
                            SymbolState state = stateMap.get(((LispSymbol) list.getItems().get(0)).getValue().toUpperCase());
                            if (state != null) {

                                String symValue = ((LispSymbol) list.getItems().get(1)).getValue().toUpperCase();
                                boolean changed = false;
                                state.timestamp = System.currentTimeMillis();
                                switch (symValue) {
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

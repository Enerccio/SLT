package com.en_circle.slt.plugin.services.lisp.components;

import com.en_circle.slt.plugin.SymbolState;
import com.en_circle.slt.plugin.SymbolState.SymbolBinding;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.plugin.swank.components.SourceLocation;
import com.en_circle.slt.plugin.swank.requests.EvalAndGrab;
import com.en_circle.slt.tools.SltApplicationUtils;
import com.google.common.collect.Lists;
import com.intellij.openapi.project.Project;
import com.intellij.util.Consumer;
import com.intellij.util.concurrency.FutureResult;
import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class SltLispEnvironmentSymbolCache extends Thread {

    // TODO: Rewrite with events, if possible

    private final Map<String, SymbolState> symbolInformation = Collections.synchronizedMap(new HashMap<>());
    private final List<SymbolState> symbolRefreshQueue = Collections.synchronizedList(new ArrayList<>());

    private final Project project;
    private volatile boolean active = true;

    public SltLispEnvironmentSymbolCache(Project project) {
        this.project = project;

        setDaemon(true);
        setName("SBCL Symbol Cache Thread");
    }

    @Override
    public void run() {
        while (active) {
            try {
                if (LispEnvironmentService.getInstance(project).getState() == LispEnvironmentState.READY) {
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

    public void terminate() {
        active = false;
    }

    public SymbolState refreshSymbolFromServer(String packageName, String symbolName) {
        SymbolState state = getOrCreateBinding(packageName, symbolName);
        SymbolState undefinedSymbol = getOrCreateBinding(null, symbolName);
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

    private void refreshBatchedSymbols(BatchedSymbolRefreshAction action, Consumer<Boolean> onFinish) {
        HashSet<SymbolState> duplicityState = new HashSet<>();
        List<SymbolState> withoutDuplicity = action.states.stream()
                .filter(s -> {
                    boolean isDuplicit = duplicityState.contains(s);
                    duplicityState.add(s);
                    return !isDuplicit;
                }).filter(this::removeBad).collect(Collectors.toList());

        try {
            if (LispEnvironmentService.getInstance(project).getState() == LispEnvironmentState.READY) {
                refreshSymbolsBatched(withoutDuplicity, onFinish);
            } else {
                onFinish.consume(true);
            }
        } catch (Exception e) {
            onFinish.consume(false);
        }
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
        refreshSymbolsBatched(refreshStates, null);
    }

    private void refreshSymbolsBatched(List<SymbolState> refreshStates, Consumer<Boolean> requestFinished) throws Exception {
        String request = "(" +
                refreshStates.stream().map(x -> x.name.toUpperCase() + " ").collect(Collectors.joining()) + ")";
        request = StringUtils.replace(request, "\"", "\\\"");

        LispEnvironmentService.getInstance(project).sendToLisp(EvalAndGrab.eval(
                String.format(
                        "(slt-core:analyze-symbols (slt-core:read-fix-packages \"%s\"))",
                        request),
                LispEnvironmentService.getInstance(project).getGlobalPackage(), true, (result, stdout, parsed) -> {
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
                                state.timestamp = System.currentTimeMillis();
                                switch (symValue) {
                                    case ":CLASS":
                                        state.binding = SymbolBinding.CLASS;
                                        break;
                                    case ":METHOD":
                                        state.binding = SymbolBinding.METHOD;
                                        break;
                                    case ":SPECIAL-FORM":
                                        state.binding = SymbolBinding.SPECIAL_FORM;
                                        break;
                                    case ":MACRO":
                                        state.binding = SymbolBinding.MACRO;
                                        break;
                                    case ":FUNCTION":
                                        state.binding = SymbolBinding.FUNCTION;
                                        break;
                                    case ":CONSTANT":
                                        state.binding = SymbolBinding.CONSTANT;
                                        break;
                                    case ":KEYWORD":
                                        state.binding = SymbolBinding.KEYWORD;
                                        break;
                                    case ":SPECIAL":
                                        state.binding = SymbolBinding.SPECIAL_VARIABLE;
                                        break;
                                    default:
                                        state.binding = SymbolBinding.NONE;
                                        break;
                                }
                                state.documentation = null;
                                if (list.getItems().get(2) instanceof LispString) {
                                    state.documentation = ((LispString) list.getItems().get(2)).getValue();
                                }

                                if (list.getItems().get(3) instanceof LispContainer) {
                                    SourceLocation location = new SourceLocation((LispContainer) list.getItems().get(3));
                                    if (!state.location.equals(location)) {
                                        state.location = location;
                                    }
                                }
                            }
                        }
                    }

                    if (requestFinished != null) {
                        requestFinished.consume(true);
                    }
                }), false, () -> {
            if (requestFinished != null) {
                requestFinished.consume(false);
            }
        });
    }

    public void clear() {
        symbolInformation.clear();
        symbolRefreshQueue.clear();
    }

    public BatchedSymbolRefreshAction createNewBatch() {
        return new BatchedSymbolRefreshAction();
    }

    public class BatchedSymbolRefreshAction {

        private final List<SymbolState> states = new ArrayList<>();

        public BatchedSymbolRefreshAction() {

        }

        public boolean getResult() {
            return SltApplicationUtils.processAsync(() -> {
                FutureResult<Boolean> waitForResult = new FutureResult<>();
                refreshBatchedSymbols(this, waitForResult::set);
                try {
                    return waitForResult.get(200, TimeUnit.SECONDS);
                } catch (Exception e) {
                    return false;
                }
            });
        }

        public void add(String name, String packageName) {
            states.add(getOrCreateBinding(packageName, name));
            states.add(getOrCreateBinding(null, name));
        }

    }


}

package com.en_circle.slt.plugin.swank.debug;

import com.en_circle.slt.plugin.lisp.lisp.*;

import java.util.ArrayList;
import java.util.List;

public class SltDebugAction {

    private final String actionName;
    private final String actionDescription;
    private final List<SltDebugArgument> argumentList = new ArrayList<>();

    public SltDebugAction(LispContainer source) {
        this.actionName = ((LispString) source.getItems().get(0)).getValue();
        this.actionDescription = ((LispString) source.getItems().get(1)).getValue();

        LispElement element = source.getItems().get(2);
        if (element.getType() == LispElementType.CONTAINER) {
            LispContainer c = (LispContainer) element;
            boolean rest = false;
            for (LispElement e : c.getItems()) {
                String name = ((LispSymbol) e).getValue();
                if (name.equalsIgnoreCase("&rest") || name.equalsIgnoreCase("common-lisp:&rest")) {
                    rest = true;
                    continue;
                }
                SltDebugArgument a = new SltDebugArgument(name, rest);
                argumentList.add(a);
            }
        }
    }

    public List<SltDebugArgument> getArguments() {
        return List.copyOf(argumentList);
    }

    public String getActionName() {
        return actionName;
    }

    public String getActionDescription() {
        return actionDescription;
    }

    @Override
    public String toString() {
        return "SltDebugAction{" +
                "actionName='" + actionName + '\'' +
                ", actionDescription='" + actionDescription + '\'' +
                '}';
    }
}

package com.en_circle.slt.plugin.lisp.lisp;

import java.util.List;

public class LispList implements LispElement {

    private final List<LispElement> items;
    private final boolean isPair;

    public LispList(List<LispElement> items, boolean isPair) {
        this.items = List.copyOf(items);
        this.isPair = isPair;
    }

    public List<LispElement> getItems() {
        return items;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        for (int i=0; i<items.size(); i++) {
            sb.append(items.get(i));
            if (isPair && i == items.size() - 1) {
                sb.append(" . ");
            } else {
                sb.append(" ");
            }
        }
        sb.append(")");
        return sb.toString();
    }

    @Override
    public LispElementType getType() {
        return LispElementType.LIST;
    }
}

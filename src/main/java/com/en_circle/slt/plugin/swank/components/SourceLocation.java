package com.en_circle.slt.plugin.swank.components;

import com.en_circle.slt.plugin.lisp.lisp.*;

public class SourceLocation {

    private String location = "undefined";
    private int position = -1;
    private boolean isPrecise = false;
    private boolean isFile = false;

    public SourceLocation() {

    }

    public SourceLocation(LispElement srcElement) {
        try {
            LispContainer src = (LispContainer) srcElement;
            if (src.getItems().get(0).equals(new LispSymbol(":location"))) {
                for (int i=1; i<src.getItems().size(); i++) {
                    LispContainer trait = (LispContainer) src.getItems().get(i);
                    if (LispUtils.hasPValue(trait, new LispSymbol(":file"))) {
                        this.location = ((LispString) LispUtils.pvalue(trait, new LispSymbol(":file"))).getValue();
                        isFile = true;
                    } else if (LispUtils.hasPValue(trait, new LispSymbol(":buffer"))) {
                        this.location = ((LispString) LispUtils.pvalue(trait, new LispSymbol(":buffer"))).getValue();
                        isFile = true;
                    } else if (LispUtils.hasPValue(trait, new LispSymbol(":buffer-and-file"))) {
                        this.location = ((LispString) LispUtils.pvalue(trait, new LispSymbol(":buffer-and-file"))).getValue();
                        isFile = true;
                    } else if (LispUtils.hasPValue(trait, new LispSymbol(":position"))) {
                        LispInteger offset = (LispInteger)  LispUtils.pvalue(trait, new LispSymbol(":position"));
                        position = offset.getValue().intValue();
                    } else if (LispUtils.hasPValue(trait, new LispSymbol(":offset"))) {
                        LispInteger offset = (LispInteger) LispUtils.pvalue(trait, new LispSymbol(":offset"));
                        position = offset.getValue().intValue();
                    }
                }
            }
            if (isFile) {
                isPrecise = true;
            }
        } catch (Exception ignored) {

        }
    }

    public String getLocation() {
        return location;
    }

    public int getPosition() {
        return position;
    }

    public boolean isPrecise() {
        return isPrecise;
    }

    public boolean isFile() {
        return isFile;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SourceLocation location1 = (SourceLocation) o;

        if (position != location1.position) return false;
        if (isPrecise != location1.isPrecise) return false;
        if (isFile != location1.isFile) return false;
        return location.equals(location1.location);
    }

    @Override
    public int hashCode() {
        int result = location.hashCode();
        result = 31 * result + position;
        result = 31 * result + (isPrecise ? 1 : 0);
        result = 31 * result + (isFile ? 1 : 0);
        return result;
    }
}

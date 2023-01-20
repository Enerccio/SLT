package com.en_circle.slt.plugin.swank.debug;

import com.en_circle.slt.plugin.lisp.lisp.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class SltInspectedObject {
    private static final Logger log = LoggerFactory.getLogger(SltInspectedObject.class);

    private final String title;
    private final BigInteger id;
    private final List<SltInspectionElement> elements = new ArrayList<>();

    public SltInspectedObject(LispContainer container) throws Exception {
        this.title = ((LispString) LispUtils.pvalue(container, new LispSymbol(":TITLE"))).getValue();
        this.id = ((LispInteger) LispUtils.pvalue(container, new LispSymbol(":ID"))).getValue();

        LispContainer contents = (LispContainer) LispUtils.pvalue(container, new LispSymbol(":CONTENT"));
        LispContainer content = (LispContainer) contents.getItems().get(0);
        for (LispElement element : content.getItems()) {
            if (element instanceof LispString) {
                if (elements.isEmpty() || elements.get(elements.size() - 1).id != null) {
                    SltInspectionElement e = new SltInspectionElement();
                    e.text = LispUtils.unescape(((LispString) element).getValue());
                    elements.add(e);
                } else {
                    elements.get(elements.size() - 1).text += LispUtils.unescape(((LispString) element).getValue());
                }
            } else if (element instanceof LispContainer) {
                LispContainer subelement = (LispContainer) element;
                SltInspectionElement e = new SltInspectionElement();
                e.text = LispUtils.unescape(((LispString) LispUtils.pvalue(subelement, new LispSymbol(":VALUE"))).getValue());
                e.id = ((LispInteger) subelement.getItems().get(2)).getValue();
                elements.add(e);
            } else {
                log.warn("Unknown element in inspector!");
            }
        }
    }

    public String getTitle() {
        return title;
    }

    public BigInteger getId() {
        return id;
    }

    public List<SltInspectionElement> getElements() {
        return elements;
    }

    public static class SltInspectionElement {

        private String text;
        private BigInteger id = null;

        public String getText() {
            return text;
        }

        public BigInteger getId() {
            return id;
        }
    }
}

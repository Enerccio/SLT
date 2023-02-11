package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.environment.abcl.ABCLOverrides;
import com.en_circle.slt.plugin.lisp.lisp.*;
import com.en_circle.slt.plugin.services.lisp.LispSltOverrides;
import com.en_circle.slt.plugin.swank.debug.SltInspectedObject;
import com.en_circle.slt.plugin.swank.debug.SltInspectedObject.SltInspectionElement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class LispSltOverridesBase implements LispSltOverrides {
    private static final Logger log = LoggerFactory.getLogger(ABCLOverrides.class);

    @Override
    public SltInspectedObject parseInspectedObject(LispContainer container) throws Exception {
        SltInspectedObject object = new SltInspectedObject();
        object.setTitle(((LispString) LispUtils.pvalue(container, new LispSymbol(":TITLE"))).getValue());
        object.setId(((LispInteger) LispUtils.pvalue(container, new LispSymbol(":ID"))).getValue());

        LispContainer contents = (LispContainer) LispUtils.pvalue(container, new LispSymbol(":CONTENT"));
        LispContainer content = (LispContainer) contents.getItems().get(0);
        for (LispElement element : content.getItems()) {
            if (element instanceof LispString) {
                if (object.getElements().isEmpty() || object.getElements().get(object.getElements().size() - 1).getId() != null) {
                    SltInspectionElement e = new SltInspectionElement();
                    e.setText(LispUtils.unescape(((LispString) element).getValue()));
                    object.getElements().add(e);
                } else {
                    String text = object.getElements().get(object.getElements().size() - 1).getText();
                    text += LispUtils.unescape(((LispString) element).getValue());
                    object.getElements().get(object.getElements().size() - 1).setText(text);
                }
            } else if (element instanceof LispContainer subelement) {
                if (LispUtils.hasPValue(subelement, new LispSymbol(":LABEL"))) {
                    SltInspectionElement sub = new SltInspectionElement();
                    sub.setText(LispUtils.unescape(((LispString) LispUtils.pvalue(subelement, new LispSymbol(":LABEL"))).getValue()));
                    sub.setTitled(true);
                    object.getElements().add(sub);
                } else if (LispUtils.hasPValue(subelement, new LispSymbol(":VALUE"))){
                    SltInspectionElement sub = new SltInspectionElement();
                    sub.setText(LispUtils.unescape(((LispString) subelement.getItems().get(1)).getValue()));
                    sub.setId(((LispInteger) subelement.getItems().get(2)).getValue());
                    object.getElements().add(sub);
                }
            } else {
                log.warn("Unknown element in inspector!");
            }
        }

        return object;
    }

}

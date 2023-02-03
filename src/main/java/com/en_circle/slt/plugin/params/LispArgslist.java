package com.en_circle.slt.plugin.params;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispString;

public class LispArgslist {

    private final String information;

    public LispArgslist(LispElement data) {
        if (data instanceof LispString str) {
            this.information = str.getValue();
        } else {
            this.information = SltBundle.message("slt.argslist.error");
        }
    }

    public String getInformation() {
        return information;
    }

}

package com.en_circle.slt.plugin.services.lisp;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.swank.debug.SltInspectedObject;

public interface LispSltOverrides {

    SltInspectedObject parseInspectedObject(LispContainer container) throws Exception;

}

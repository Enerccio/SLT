package com.en_circle.slt.templates;

import org.apache.commons.lang3.StringUtils;
import org.watertemplate.Template;

public class SbclVerifyTemplate extends Template {

    public SbclVerifyTemplate(String quicklispPath) {
        if (quicklispPath.contains("\\")) {
            quicklispPath = StringUtils.replace(quicklispPath, "\\", "\\\\");
        }
        add("qlpath", quicklispPath);
    }

    @Override
    protected String getFilePath() {
        return "sbcl-init.cl";
    }

}

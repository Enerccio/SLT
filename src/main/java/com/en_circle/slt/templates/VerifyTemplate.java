package com.en_circle.slt.templates;

import org.apache.commons.lang3.StringUtils;
import org.watertemplate.Template;

public class VerifyTemplate extends Template {

    public VerifyTemplate(String quicklispPath) {
        if (quicklispPath.contains("\\")) {
            quicklispPath = StringUtils.replace(quicklispPath, "\\", "\\\\");
        }
        add("qlpath", quicklispPath);
    }

    @Override
    protected String getFilePath() {
        return "verify.cl";
    }

}

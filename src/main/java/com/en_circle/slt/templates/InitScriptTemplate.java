package com.en_circle.slt.templates;

import com.en_circle.slt.plugin.swank.SwankServerConfiguration;
import org.watertemplate.Template;

public class InitScriptTemplate extends Template {

    public InitScriptTemplate(SwankServerConfiguration configuration, String sltCoreScript) {
        add("qlpath", configuration.getQuicklispStartScript());
        add("port", "" + configuration.getPort());
        add("cwd", configuration.getProjectDirectory());
        add("sbclcorefile", sltCoreScript);
    }

    @Override
    protected String getFilePath() {
        return "initscript.cl";
    }
}

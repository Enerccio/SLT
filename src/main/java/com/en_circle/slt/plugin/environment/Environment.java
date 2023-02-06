package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.environment.abcl.ABCLEnvironmentDefinition;
import com.en_circle.slt.plugin.environment.clisp.CCLEnvironmentDefinition;
import com.en_circle.slt.plugin.environment.sbcl.SBCLEnvironmentDefinition;

public enum Environment {

    ABCL_PROCESS(new ABCLEnvironmentDefinition()),
    SBCL_PROCESS(new SBCLEnvironmentDefinition()),
    CCL_PROCESS(new CCLEnvironmentDefinition()),

    ;

    private final EnvironmentDefinition definition;

    Environment(EnvironmentDefinition definition) {
        this.definition = definition;
    }

    public EnvironmentDefinition getDefinition() {
        return definition;
    }

}

package com.en_circle.slt.plugin.environment;

import com.en_circle.slt.plugin.environment.abcl.ABCLEnvironmentDefinition;
import com.en_circle.slt.plugin.environment.allegro.AllegroCLEnvironmentDefinition;
import com.en_circle.slt.plugin.environment.ccl.CCLEnvironmentDefinition;
import com.en_circle.slt.plugin.environment.cmucl.CMUCLEnvironmentDefinition;
import com.en_circle.slt.plugin.environment.sbcl.SBCLEnvironmentDefinition;

public enum Environment {

    ABCL_PROCESS(new ABCLEnvironmentDefinition()),
    SBCL_PROCESS(new SBCLEnvironmentDefinition()),
    CCL_PROCESS(new CCLEnvironmentDefinition()),
    ALLEGRO_PROCESS(new AllegroCLEnvironmentDefinition()),
    CMUCL_PROCESS(new CMUCLEnvironmentDefinition()),

    ;

    private final EnvironmentDefinition definition;

    Environment(EnvironmentDefinition definition) {
        this.definition = definition;
    }

    public EnvironmentDefinition getDefinition() {
        return definition;
    }

}

package com.en_circle.slt.plugin.ui.debug;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.requests.EvalStringInFrameEval;
import com.en_circle.slt.plugin.ui.console.SltConsole;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;

public class SltFrameConsole extends SltConsole {
    private static final Logger log = LoggerFactory.getLogger(SltFrameConsole.class);

    private final BigInteger threadId;
    private final BigInteger frame;
    private final Runnable onChange;

    public SltFrameConsole(Project project, BigInteger threadId, BigInteger frame, Runnable onChange, String module) {
        super(project);
        this.threadId = threadId;
        this.frame = frame;
        this.onChange = onChange;
        this.currentPackage = module;
    }

    @Override
    protected void eval(String data) {
        try {
            if (StringUtils.isNotBlank(data)) {
                LispEnvironmentService.getInstance(project).sendToLisp(EvalStringInFrameEval.evalInFrame(data, frame, threadId, currentPackage,
                        result -> {
                            languageConsole.print(result + "\n", ConsoleViewContentType.NORMAL_OUTPUT);
                            onChange.run();
                        }));
            }
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.sbclstart"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.sbcl.start"));
        }
    }

    @Override
    public String getTitle() {
        return SltBundle.message("slt.ui.debugger.frame.repl");
    }
}
